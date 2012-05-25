{------------------------------------------------------------------------------}
{                                                                              }
{  WaveOut - Abstract definition of wave audio output                          }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

{$I DELPHIAREA.INC}

unit WaveOut;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Windows, Messages, Classes, mmSystem, WaveUtils, WaveIO;

type

  // The base abstract class for wave audio player components
  TWaveAudioOut = class(TWaveAudioIO)
  private
    fHandle: HWAVEOUT;
    fPaused: Boolean;
    fVolumeLeft: WORD;
    fVolumeRight: WORD;
    fPitch: Double;
    fPlaybackRate: Double;
    fOptions: TWaveOutOptions;
    fBufferInternally: Boolean;
    fStartPosition: DWORD;
    function GetDeviceSupports: TWaveOutDeviceSupports;
    procedure SetOptions(const Value: TWaveOutOptions);
    function GetVolume: WORD;
    procedure SetVolume(Value: WORD);
    function GetVolumeLeft: WORD;
    procedure SetVolumeLeft(Value: WORD);
    function GetVolumeRight: WORD;
    procedure SetVolumeRight(Value: WORD);
    function GetPitch: Double;
    procedure SetPitch(const Value: Double);
    function GetPlaybackRate: Double;
    procedure SetPlaybackRate(const Value: Double);
    function IsPitchStored: Boolean;
    function IsPlaybackRateStored: Boolean;
    procedure AdjustOptionItems;
  protected
    procedure DoWaveOutDeviceOpen; override;
    procedure DoWaveOutDeviceClose; override;
    procedure DoWaveOutDeviceDone(pWaveHeader: PWaveHdr); override;
    function GetChannelVolumes(var Left, Right: WORD): Boolean; virtual;
    function SetChannelVolumes(var Left, Right: WORD): Boolean; virtual;
    function GetNumDevs: DWORD; override;
    function GetPaused: Boolean; override;
    function GetDeviceName: String; override;
    function GetDeviceFormats: TWaveDeviceFormats; override;
    function GetPosition: DWORD; override;
    procedure SetPosition(Value: DWORD); virtual;
    function GetErrorText(ErrorCode: MMRESULT): String; override;
    function ValidateDeviceID(ADeviceID: DWORD): MMRESULT; override;
    procedure DefineBuffers; override;
    function InternalOpen: Boolean; override;
    function InternalClose: Boolean; override;
    function InternalPause: Boolean; override;
    function InternalResume: Boolean; override;
    function HandleAllocated: Boolean; override;
    function WriteWaveHeader(const pWaveHeader: PWaveHdr): Boolean; virtual;
    function WriteBuffer(const Buffer: Pointer; BufferSize: DWORD;
      NumLoops: DWORD; FreeIt: Boolean): Boolean; virtual;
    function GetWaveData(const Buffer: Pointer; BufferSize: DWORD;
      var NumLoops: DWORD): DWORD; virtual;
    function GetWaveDataPtr(var Buffer: Pointer;
      var NumLoops: DWORD; var FreeIt: Boolean): DWORD; virtual;
    property StartPosition: DWORD read fStartPosition;
  protected
    property DeviceSupports: TWaveOutDeviceSupports read GetDeviceSupports;
    property Options: TWaveOutOptions read fOptions write SetOptions default [];
    property Volume: WORD read GetVolume write SetVolume stored False;              // Percent (Both Channels)
    property VolumeLeft: WORD read GetVolumeLeft write SetVolumeLeft default 75;    // Percent (Left Channel)
    property VolumeRight: WORD read GetVolumeRight write SetVolumeRight default 75; // Percent (Right Channel)
    property Pitch: Double read GetPitch write SetPitch stored IsPitchStored;
    property PlaybackRate: Double read GetPlaybackRate write SetPlaybackRate stored IsPlaybackRateStored;
    property BufferInternally: Boolean read fBufferInternally write fBufferInternally default True;
    property Position: DWORD read GetPosition write SetPosition; // Milliseconds
  public
    constructor Create(AOwner: TComponent); override;
    function Query(const pWaveFormat: PWaveFormatEx): Boolean; override;
    property Handle: HWAVEOUT read fHandle;
  end;

implementation

uses
  SysUtils;

{ Helper Functions }

procedure DW2PercentVolume(dwVal: DWORD; var wLeft, wRight: WORD);
begin
  wLeft := MulDiv(LoWord(dwVal), 100, $FFFF);
  wRight := MulDiv(HiWord(dwVal), 100, $FFFF);
end;

function Percent2DWVolume(wLeft, wRight: WORD): DWORD;
begin
  Result := MakeLong(MulDiv(wLeft, $FFFF, 100), MulDiv(wRight, $FFFF, 100));
end;

function Float2DW(ftVal: Double): DWORD;
var
  HW, LW: WORD;
  D: Double;
  I: Integer;
begin
  HW := Trunc(ftVal);
  D := Frac(ftVal);
  LW := 0;
  for I := 1 to 16 do
  begin
    LW := LW shl 1;
    D := 2 * D;
    if ftVal >= 1 then
    begin
      LW := LW or $0001;
      D := Frac(D);
    end;
  end;
  Result := MakeLong(LW, HW);
end;

function DW2Float(dwVal: DWORD): Double;
var
  LW: WORD;
  D: Double;
begin
  Result := SmallInt(HiWord(dwVal));
  LW := LoWord(dwVal);
  D := 1;
  while LW <> 0 do
  begin
    D := 2 * D;
    if WordBool(LW and $8000) then
      Result := Result + (1 / D);
    LW := LW shl 1;
  end;
end;

{ TWaveAudioOut }

constructor TWaveAudioOut.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fOptions := [];
  fVolumeLeft := 75;
  fVolumeRight := 75;
  fPitch := 1.0;
  fPlaybackRate := 1.0;
  fBufferInternally := True;
end;

function TWaveAudioOut.GetPaused: Boolean;
begin
  Result := fPaused;
end;

function TWaveAudioOut.GetNumDevs: DWORD;
begin
  Result := waveOutGetNumDevs;
end;

function TWaveAudioOut.GetDeviceName: String;
var
  DevCaps: TWaveOutCaps;
begin
  if waveOutGetDevCaps(DeviceID, @DevCaps, SizeOf(DevCaps)) = MMSYSERR_NOERROR then
    Result := StrPas(DevCaps.szPname)
  else
    Result := '';
end;

function TWaveAudioOut.GetDeviceFormats: TWaveDeviceFormats;
var
  DevCaps: TWaveOutCaps;
begin
  Result := [];
  if waveOutGetDevCaps(DeviceID, @DevCaps, SizeOf(DevCaps)) = MMSYSERR_NOERROR then
  begin
    Include(Result, Mono8bit8000Hz);
    Include(Result, Stereo8bit8000Hz);
    Include(Result, Mono16bit8000Hz);
    Include(Result, Stereo16bit8000Hz);
    if LongBool(DevCaps.dwFormats and WAVE_FORMAT_1M08) then
      Include(Result, Mono8bit11025Hz);
    if LongBool(DevCaps.dwFormats and WAVE_FORMAT_1S08) then
      Include(Result, Stereo8bit11025Hz);
    if LongBool(DevCaps.dwFormats and WAVE_FORMAT_1M16) then
      Include(Result, Mono16bit11025Hz);
    if LongBool(DevCaps.dwFormats and WAVE_FORMAT_1S16) then
      Include(Result, Stereo16bit11025Hz);
    if LongBool(DevCaps.dwFormats and WAVE_FORMAT_2M08) then
      Include(Result, Mono8bit22050Hz);
    if LongBool(DevCaps.dwFormats and WAVE_FORMAT_2S08) then
      Include(Result, Stereo8bit22050Hz);
    if LongBool(DevCaps.dwFormats and WAVE_FORMAT_2M16) then
      Include(Result, Mono16bit22050Hz);
    if LongBool(DevCaps.dwFormats and WAVE_FORMAT_2S16) then
      Include(Result, Stereo16bit22050Hz);
    if LongBool(DevCaps.dwFormats and WAVE_FORMAT_4M08) then
      Include(Result, Mono8bit44100Hz);
    if LongBool(DevCaps.dwFormats and WAVE_FORMAT_4S08) then
      Include(Result, Stereo8bit44100Hz);
    if LongBool(DevCaps.dwFormats and WAVE_FORMAT_4M16) then
      Include(Result, Mono16bit44100Hz);
    if LongBool(DevCaps.dwFormats and WAVE_FORMAT_4S16) then
      Include(Result, Stereo16bit44100Hz);
  end;
end;

function TWaveAudioOut.GetDeviceSupports: TWaveOutDeviceSupports;
var
  DevCaps: TWaveOutCaps;
begin
  Result := [];
  if waveOutGetDevCaps(DeviceID, @DevCaps, SizeOf(DevCaps)) = MMSYSERR_NOERROR then
  begin
    Include(Result, dsAsynchronize);
    if LongBool(DevCaps.dwSupport and WAVECAPS_VOLUME) then
    begin
      Include(Result, dsVolume);
      if LongBool(DevCaps.dwSupport and WAVECAPS_LRVOLUME) then
        Include(Result, dsStereoVolume);
    end;
    if LongBool(DevCaps.dwSupport and WAVECAPS_PITCH) then
      Include(Result, dsPitch);
    if LongBool(DevCaps.dwSupport and WAVECAPS_PLAYBACKRATE) then
      Include(Result, dsPlaybackRate);
    if LongBool(DevCaps.dwSupport and WAVECAPS_SAMPLEACCURATE) then
      Include(Result, dsPosition);
    if LongBool(DevCaps.dwSupport and WAVECAPS_SYNC) then
      Exclude(Result, dsAsynchronize);
    if LongBool(DevCaps.dwSupport and WAVECAPS_DIRECTSOUND) then
      Exclude(Result, dsDirectSound);
  end;
end;

function TWaveAudioOut.GetChannelVolumes(var Left, Right: WORD): Boolean;
var
  Supports: TWaveOutDeviceSupports;
  V: DWORD;
begin
  Result := False;
  Supports := DeviceSupports;
  if HandleAllocated and (dsVolume in Supports) then
  begin
    if waveOutGetVolume(Handle, @V) = MMSYSERR_NOERROR then
    begin
      DW2PercentVolume(V, Left, Right);
      if not (dsStereoVolume in Supports) then
        Right := Left;
      Result := True;
    end;
  end;
end;

function TWaveAudioOut.SetChannelVolumes(var Left, Right: WORD): Boolean;
var
  V: WORD;
  Supports: TWaveOutDeviceSupports;
begin
  Result := False;
  Supports := DeviceSupports;
  if (woSetVolume in Options) and HandleAllocated and (dsVolume in Supports) then
  begin
    if not (dsStereoVolume in Supports) then
    begin
      V := (Left + Right) div 2;
      Left := V;
      Right := V;
    end;
    if waveOutSetVolume(Handle, Percent2DWVolume(Left, Right)) = MMSYSERR_NOERROR then
      Result := True;
  end;
end;

function TWaveAudioOut.GetVolume: WORD;
begin
  GetChannelVolumes(fVolumeLeft, fVolumeRight);
  Result := (fVolumeLeft + fVolumeRight) div 2;
end;

procedure TWaveAudioOut.SetVolume(Value: WORD);
begin
  fVolumeLeft := Value;
  fVolumeRight := Value;
  SetChannelVolumes(fVolumeLeft, fVolumeRight);
end;

function TWaveAudioOut.GetVolumeLeft: WORD;
begin
  GetChannelVolumes(fVolumeLeft, fVolumeRight);
  Result := fVolumeLeft;
end;

procedure TWaveAudioOut.SetVolumeLeft(Value: WORD);
begin
  fVolumeLeft := Value;
  SetChannelVolumes(fVolumeLeft, fVolumeRight);
end;

function TWaveAudioOut.GetVolumeRight: WORD;
begin
  GetChannelVolumes(fVolumeLeft, fVolumeRight);
  Result := fVolumeRight;
end;

procedure TWaveAudioOut.SetVolumeRight(Value: WORD);
begin
  fVolumeRight := Value;
  SetChannelVolumes(fVolumeLeft, fVolumeRight);
end;

function TWaveAudioOut.GetPitch: Double;
var
  Value: DWORD;
begin
  if HandleAllocated then
  begin
    if dsPitch in DeviceSupports then
    begin
      waveOutGetPitch(Handle, @Value);
      Result := DW2Float(Value);
    end
    else
      Result := 1.0;
  end
  else
    Result := fPitch;
end;

procedure TWaveAudioOut.SetPitch(const Value: Double);
begin
  if fPitch <> Value then
  begin
    fPitch := Value;
    if HandleAllocated and (woSetPitch in Options) and (dsPitch in DeviceSupports) then
      waveOutSetPitch(Handle, Float2DW(fPitch));
  end;
end;

function TWaveAudioOut.GetPlaybackRate: Double;
var
  Value: DWORD;
begin
  if HandleAllocated then
  begin
    if dsPlaybackRate in DeviceSupports then
    begin
      waveOutGetPlaybackRate(Handle, @Value);
      Result := DW2Float(Value);
    end
    else
      Result := 1.0;
  end
  else
    Result := fPlaybackRate;
end;

procedure TWaveAudioOut.SetPlaybackRate(const Value: Double);
begin
  if fPlaybackRate <> Value then
  begin
    fPlaybackRate := Value;
    if HandleAllocated and (woSetPlaybackRate in Options) and (dsPlaybackRate in DeviceSupports) then
      waveOutSetPitch(Handle, Float2DW(fPlaybackRate));
  end;
end;

procedure TWaveAudioOut.SetOptions(const Value: TWaveOutOptions);
begin
  if Options <> Value then
  begin
    fOptions := Value;
    if HandleAllocated then AdjustOptionItems;
  end;
end;

function TWaveAudioOut.GetPosition: DWORD;
var
  mmTime: TMMTime;
begin
  Result := StartPosition;
  mmTime.wType := TIME_MS;
  if WaveOutGetPosition(Handle, @mmTime, SizeOf(mmTime)) = MMSYSERR_NOERROR then
    Inc(Result, mmTimeToMS(mmTime));
end;

procedure TWaveAudioOut.SetPosition(Value: DWORD);
begin
  fStartPosition := Value;
  if HandleAllocated then
  begin
    waveOutReset(Handle);
    fPaused := Paused and Success(waveOutPause(Handle));
  end;
end;

function TWaveAudioOut.GetErrorText(ErrorCode: MMRESULT): String;
var
  ErrorText: array[0..255] of Char;
begin
  if waveOutGetErrorText(ErrorCode, ErrorText, SizeOf(ErrorText)) = MMSYSERR_NOERROR then
    Result := StrPas(ErrorText)
  else
    Result := '';
end;

function TWaveAudioOut.IsPitchStored: Boolean;
begin
  Result := (fPitch <> 1.0);
end;

function TWaveAudioOut.IsPlaybackRateStored: Boolean;
begin
  Result := (fPlaybackRate <> 1.0);
end;

procedure TWaveAudioOut.AdjustOptionItems;
var
  Supports: TWaveOutDeviceSupports;
begin
  Supports := DeviceSupports;
  if (woSetVolume in Options) and (dsVolume in Supports) then
    waveOutSetVolume(Handle, Percent2DWVolume(fVolumeLeft, fVolumeRight));
  if (woSetPitch in Options) and (dsPitch in Supports) then
    waveOutSetPitch(Handle, Float2DW(fPitch));
  if (woSetPlaybackRate in Options) and (dsPlaybackRate in Supports) then
    waveOutSetPlaybackRate(Handle, Float2DW(fPlaybackRate));
end;

function TWaveAudioOut.ValidateDeviceID(ADeviceID: DWORD): MMRESULT;
var
  DevCaps: TWaveOutCaps;
begin
  Result := waveOutGetDevCaps(ADeviceID, @DevCaps, SizeOf(DevCaps));
end;

function TWaveAudioOut.InternalOpen: Boolean;
var
  pWaveFormat: PWaveFormatEx;
  FreeWaveFormat: Boolean;
begin
  Result := False;
  if not Opening then
  begin
    if not Active then
    begin
      if Closing then
        WaitForStop;
      Lock;
      Opening := True;
      try
        FreeWaveFormat := True;
        GetWaveFormat(pWaveFormat, FreeWaveFormat);
        // <<-- Nashev: Wave may be empty
        if pWaveFormat <> nil then
        // -->>
        try
          if Success(WaveOutOpen(nil, DeviceID, pWaveFormat, 0, 0, WAVE_FORMAT_QUERY)) then
          begin
            Move(pWaveFormat^, WaveFormat, SizeOf(WaveFormat) - SizeOf(WaveFormat.cbSize));
            CreateCallback;
            try
              if Success(WaveOutOpen(@fHandle, DeviceID, pWaveFormat, Callback, 0, CallbackType)) then
                Result := True
              else
                DestroyCallback;
            except
              DestroyCallback;
            end;
          end;
        finally
          if FreeWaveFormat then
            FreeMem(pWaveFormat);
        end;
      finally
        Opening := False;
        Unlock;
      end;
    end
    else
      raise EWaveAudioInvalidOperation.Create('Device is aleardy open');
  end;
end;

function TWaveAudioOut.InternalClose: Boolean;
begin
  Result := False;
  if not Closing then
  begin
    if Opening then
      WaitForStart;
    if Active then
    begin
      Lock;
      try
        Closing := True;
        try
          if Success(WaveOutReset(Handle)) then
            if ActiveBufferCount = 0 then
              Result := Success(WaveOutClose(Handle))
            else
              Result := True
          else
            Closing := False;
        except
          Closing := False;
          raise;
        end;
      finally
        Unlock;
      end;
    end
    else
      raise EWaveAudioInvalidOperation.Create('Device is aleardy close');
  end;
end;

function TWaveAudioOut.InternalPause: Boolean;
begin
  Result := False;
  if not Paused then
  begin
    Lock;
    try
      if not HandleAllocated or Success(WaveOutPause(Handle)) then
      begin
        fPaused := True;
        DoPause;
        Result := True;
      end;
    finally
      Unlock;
    end;
  end;
end;

function TWaveAudioOut.InternalResume: Boolean;
begin
  Result := False;
  if Paused then
  begin
    Lock;
    try
      if not HandleAllocated or Success(WaveOutRestart(Handle)) then
      begin
        fPaused := False;
        DoResume;
        Result := True;
      end;
    finally
      Unlock;
    end;
 end;
end;

function TWaveAudioOut.HandleAllocated: Boolean;
begin
  Result := (Handle <> 0);
end;

function TWaveAudioOut.WriteWaveHeader(const pWaveHeader: PWaveHdr): Boolean;
var
  AlreadyPrepared: Boolean;
begin
  Result := False;
  AlreadyPrepared := LongBool(pWaveHeader^.dwFlags and WHDR_PREPARED);
  if AlreadyPrepared or
     Success(waveOutPrepareHeader(Handle, pWaveHeader, SizeOf(TWaveHdr)))
  then
    try
      DoFilter(pWaveHeader^.lpData, pWaveHeader^.dwBufferLength);
      DoLevel(pWaveHeader^.lpData, pWaveHeader^.dwBufferLength);
      if Success(waveOutWrite(Handle, pWaveHeader, SizeOf(TWaveHdr))) then
        Result := True
      else if not AlreadyPrepared then
        waveOutUnprepareHeader(Handle, pWaveHeader, SizeOf(TWaveHdr));
    except
      if not AlreadyPrepared then
        waveOutUnprepareHeader(Handle, pWaveHeader, SizeOf(TWaveHdr));
      raise;
    end;
end;

function TWaveAudioOut.WriteBuffer(const Buffer: Pointer; BufferSize: DWORD;
  NumLoops: DWORD; FreeIt: Boolean): Boolean;
var
  pWaveHeader: PWaveHdr;
begin
  Result := False;
  pWaveHeader := nil;
  if ReallocateBuffer(pWaveHeader, BufferSize, Buffer) then
  begin
    if FreeIt then
      pWaveHeader^.dwUser := DWORD(Self);
    if NumLoops <> 0 then
    begin
      pWaveHeader^.dwFlags := WHDR_BEGINLOOP or WHDR_BEGINLOOP;
      pWaveHeader^.dwLoops := NumLoops;
    end;
    try
      if WriteWaveHeader(pWaveHeader) then
        Result := True
      else
        ReallocateBuffer(pWaveHeader, 0, nil);
    except
      ReallocateBuffer(pWaveHeader, 0, nil);
    end;
  end;
end;

function TWaveAudioOut.GetWaveDataPtr(var Buffer: Pointer;
  var NumLoops: DWORD; var FreeIt: Boolean): DWORD;
begin
  Result := 0;
end;

function TWaveAudioOut.GetWaveData(const Buffer: Pointer;
  BufferSize: DWORD; var NumLoops: DWORD): DWORD;
begin
  Result := 0;
end;

function TWaveAudioOut.Query(const pWaveFormat: PWaveFormatEx): Boolean;
begin
  Result := (WaveOutOpen(nil, DeviceID, pWaveFormat, 0, 0,
    WAVE_FORMAT_QUERY) = MMSYSERR_NOERROR);
end;

procedure TWaveAudioOut.DefineBuffers;
begin
  if (ActiveBufferCount = 0) and HandleAllocated and not Closing then
    PostWaveMessage(MM_WOM_DONE, nil);
end;

procedure TWaveAudioOut.DoWaveOutDeviceOpen;
begin
  AdjustOptionItems;
  fPaused := Paused and Success(waveOutPause(Handle));
  inherited DoWaveOutDeviceOpen;
end;

procedure TWaveAudioOut.DoWaveOutDeviceClose;
begin
  fHandle := 0;
  fStartPosition := 0;
  inherited DoWaveOutDeviceClose;
end;

procedure TWaveAudioOut.DoWaveOutDeviceDone(pWaveHeader: PWaveHdr);
var
  DataSize: DWORD;
  NumLoops: DWORD;
  Buffer: Pointer;
  FreeBuffer: Boolean;
  MakeSilence: Boolean;
begin
  try
    try
      if Assigned(pWaveHeader) then
        Success(waveOutUnprepareHeader(Handle, pWaveHeader, SizeOf(TWaveHdr)));
      if not Closing and (ActiveBufferCount <= BufferCount) then
      begin
        DataSize := 0;
        NumLoops := 0;
        if BufferInternally then
        begin
          if ReallocateBuffer(pWaveHeader, PreferredBufferSize, nil) then
          begin
            DataSize := GetWaveData(pWaveHeader^.lpData, pWaveHeader^.dwBufferLength, NumLoops);
            if DataSize < pWaveHeader^.dwBufferLength then
              ReallocateBuffer(pWaveHeader, DataSize, nil);
          end;
          Closing := (DataSize = 0);
        end
        else
        begin
          Buffer := nil;
          FreeBuffer := True;
          MakeSilence := False;
          DataSize := GetWaveDataPtr(Buffer, NumLoops, FreeBuffer);
          if not Assigned(Buffer) and (DataSize <> 0) then
          begin
            MakeSilence := True;
            if ActiveBufferCount <= 1 then
            begin
              FreeBuffer := True;
              DataSize := CalcWaveBufferSize(@WaveFormat, DataSize {Silence Duration})
            end
            else
              DataSize := 0;
          end;
          ReallocateBuffer(pWaveHeader, DataSize, Buffer);
          if Assigned(pWaveHeader) and FreeBuffer then
          begin
            pWaveHeader^.dwUser := DWORD(Self);
            if MakeSilence then
              SilenceWaveAudio(pWaveHeader^.lpData, pWaveHeader^.dwBufferLength, @WaveFormat);
          end;
          Closing := (DataSize = 0) and not MakeSilence;
        end;
        if not Closing and Assigned(pWaveHeader) then
        begin
          if NumLoops <> 0 then
          begin
            pWaveHeader^.dwFlags := WHDR_BEGINLOOP or WHDR_ENDLOOP;
            pWaveHeader^.dwLoops := NumLoops;
          end;
          WriteWaveHeader(pWaveHeader);
          if ActiveBufferCount < BufferCount then
            PostWaveMessage(MM_WOM_DONE, nil);
        end;
      end;
    finally
      if Closing and Assigned(pWaveHeader) then
      begin
        if LongBool(pWaveHeader^.dwFlags and WHDR_PREPARED) then
          waveOutUnprepareHeader(Handle, pWaveHeader, SizeOf(TWaveHdr));
        ReallocateBuffer(pWaveHeader, 0, nil);
      end;
    end;
  finally
    if Closing and (ActiveBufferCount = 0) then
      Success(WaveOutClose(Handle));
  end;
end;

end.
