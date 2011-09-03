{------------------------------------------------------------------------------}
{                                                                              }
{  WavePlayers - Wave player components                                        }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

{$I DELPHIAREA.INC}

unit WavePlayers;

interface

uses
  Windows, Messages, Classes, mmSystem, WaveUtils, WaveStorage, WaveOut;

type

  // Plays wave from a wave stream
  TAudioPlayer = class(TWaveAudioOut)
  private
    fWave: TWaveStreamAdapter;
    procedure SetWave(Value: TWaveStreamAdapter);
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
    procedure WaveChanged(Sender: TObject);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function CreateWave: TWaveStreamAdapter; virtual;
    procedure SetPosition(Value: DWORD); override;
    procedure GetWaveFormat(var pWaveFormat: PWaveFormatEx;
      var FreeIt: Boolean); override;
    function GetWaveData(const Buffer: Pointer; BufferSize: DWORD;
      var NumLoops: DWORD): DWORD; override;
    procedure DoWaveOutDeviceClose; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property NumDevs;
    property DeviceName;
    property DeviceFormats;
    property DeviceSupports;
    property LastError;
    property LastErrorText;
    property Position;          // Milliseconds
    property DeviceID;
    property Paused;
    property Active;
  published
    property Wave: TWaveStreamAdapter read fWave write SetWave;
    property Options;
    property Volume;            // Percent (Both channels)
    property VolumeLeft;        // Percent (Left channel)
    property VolumeRight;       // Percent (Right channel)
    property Pitch;
    property PlaybackRate;
    property BufferLength;      // Milliseconds
    property BufferCount;
    property Async;
    property OnActivate;
    property OnDeactivate;
    property OnPause;
    property OnResume;
    property OnError;
    property OnLevel;
  end;

  // Plays wave from user defined buffer
  TLiveAudioPlayer = class(TWaveAudioOut)
  private
    fPCMFormat: TPCMFormat;
    fOnFormat: TWaveAudioGetFormatEvent;
    fOnData: TWaveAudioGetDataEvent;
    fOnDataPtr: TWaveAudioGetDataPtrEvent;
    procedure SetPCMFormat(Value: TPCMFormat);
  protected
    procedure SetPosition(Value: DWORD); override;
    procedure GetWaveFormat(var pWaveFormat: PWaveFormatEx;
      var FreeIt: Boolean); override;
    function GetWaveData(const Buffer: Pointer; BufferSize: DWORD;
      var NumLoops: DWORD): DWORD; override;
    function GetWaveDataPtr(var Buffer: Pointer;
      var NumLoops: DWORD; var FreeIt: Boolean): DWORD; override;
  public
    constructor Create(AOwner: TComponent); override;
    function BreakLoop: Boolean;
    property PreferredBufferSize;
    property NumDevs;
    property DeviceName;
    property DeviceFormats;
    property DeviceSupports;
    property LastError;
    property LastErrorText;
    property Position;          // Milliseconds
    property DeviceID;
    property Paused;
    property Active;
  published
    property PCMFormat: TPCMFormat read fPCMFormat write SetPCMFormat default Mono16Bit8000Hz;
    property Options;
    property Volume;            // Percent (Both channels)
    property VolumeLeft;        // Percent (Left channel)
    property VolumeRight;       // Percent (Right channel)
    property Pitch;
    property PlaybackRate;
    property BufferInternally;
    property BufferLength;      // Milliseconds
    property BufferCount;
    property Async;
    property OnActivate;
    property OnDeactivate;
    property OnPause;
    property OnResume;
    property OnError;
    property OnLevel;
    property OnFormat: TWaveAudioGetFormatEvent read fOnFormat write fOnFormat;
    property OnData: TWaveAudioGetDataEvent read fOnData write fOnData;
    property OnDataPtr: TWaveAudioGetDataPtrEvent read fOnDataPtr write fOnDataPtr;
  end;

  // Plays wave as file, resource, or stream
  TStockAudioPlayer = class(TWaveAudioOut)
  private
    Wave: TWaveStreamAdapter;
    fStock: TCustomWaveStorage;
    procedure SetStock(Value: TCustomWaveStorage);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetActive(Value: Boolean); override;
    procedure SetPosition(Value: DWORD); override;
    procedure GetWaveFormat(var pWaveFormat: PWaveFormatEx;
      var FreeIt: Boolean); override;
    function GetWaveData(const Buffer: Pointer; BufferSize: DWORD;
      var NumLoops: DWORD): DWORD; override;
    procedure DoWaveOutDeviceClose; override;
    procedure DoError; override;
    function InternalPlay(AStream: TStream; Ownership: TStreamOwnership): Boolean; virtual;
  public
    function PlayStream(AStream: TStream): Boolean;
    function PlayFile(const FileName: String): Boolean;
    function PlayResName(Instance: THandle; const ResName: String): Boolean;
    function PlayResID(Instance: THandle; ResID: Integer): Boolean;
    function PlayStock(Index: Integer): Boolean;
    function Stop: Boolean;
    property NumDevs;
    property DeviceName;
    property DeviceFormats;
    property DeviceSupports;
    property LastError;
    property LastErrorText;
    property Position;          // Milliseconds
    property DeviceID;
    property Paused;
    property Active;
  published
    property Stock: TCustomWaveStorage read fStock write SetStock;
    property Options;
    property Volume;            // Percent (Both channels)
    property VolumeLeft;        // Percent (Left channel)
    property VolumeRight;       // Percent (Right channel)
    property Pitch;
    property PlaybackRate;
    property BufferLength;      // Milliseconds
    property BufferCount;
    property Async;
    property OnActivate;
    property OnDeactivate;
    property OnPause;
    property OnResume;
    property OnError;
    property OnLevel;
  end;

implementation

uses
  SysUtils;

{ TAudioPlayer }

constructor TAudioPlayer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fWave := CreateWave;
  fWave.OnChange := WaveChanged;
end;

destructor TAudioPlayer.Destroy;
begin
  inherited Destroy;
  fWave.Free;
end;

procedure TAudioPlayer.WaveChanged(Sender: TObject);
begin
  Active := False;
end;

procedure TAudioPlayer.SetWave(Value: TWaveStreamAdapter);
begin
  if Wave <> Value then
  begin
    Active := False;
    if Assigned(Value) then
    begin
      Value.Stream.Position := 0;
      Wave.LoadFromStream(Value.Stream);
    end
    else
      Wave.Clear;
  end;
end;

procedure TAudioPlayer.ReadData(Stream: TStream);
begin
  Wave.LoadFromStream(Stream);
end;

procedure TAudioPlayer.WriteData(Stream: TStream);
begin
  Wave.SaveToStream(Stream);
end;

procedure TAudioPlayer.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, not Wave.Empty);
end;

function TAudioPlayer.CreateWave: TWaveStreamAdapter;
begin
  Result := TWave.Create;
end;

procedure TAudioPlayer.GetWaveFormat(var pWaveFormat: PWaveFormatEx;
  var FreeIt: Boolean);
begin
  FreeIt := False;
  if Wave.BeginRead then
  begin
    pWaveFormat := Wave.WaveFormat;
    Wave.Position := GetWaveDataPositionOffset(pWaveFormat, StartPosition);
  end;
end;

function TAudioPlayer.GetWaveData(const Buffer: Pointer; BufferSize: DWORD;
  var NumLoops: DWORD): DWORD;
begin
  Result := Wave.Read(Buffer^, BufferSize);
end;

procedure TAudioPlayer.DoWaveOutDeviceClose;
begin
  try
    Wave.EndRead;
  finally
    inherited DoWaveOutDeviceClose;
  end;
end;

procedure TAudioPlayer.SetPosition(Value: DWORD);
begin
  if Wave.State = wssReading then
    Wave.Position := GetWaveDataPositionOffset(Wave.WaveFormat, Value);
  inherited SetPosition(Value);
end;

{ TLiveAudioPlayer }

constructor TLiveAudioPlayer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fPCMFormat := Mono16Bit8000Hz;
end;

procedure TLiveAudioPlayer.SetPCMFormat(Value: TPCMFormat);
begin
  if PCMFormat <> Value then
  begin
    if Active then
      raise EWaveAudioInvalidOperation.Create('Audio format cannot be changed while device is open')
    else
      fPCMFormat := Value;
  end;
end;

procedure TLiveAudioPlayer.GetWaveFormat(var pWaveFormat: PWaveFormatEx;
  var FreeIt: Boolean);
begin
  if PCMFormat <> nonePCM then
  begin
    GetMem(pWaveFormat, SizeOf(TWaveFormatEx));
    SetPCMAudioFormatS(pWaveFormat, PCMFormat)
  end
  else if Assigned(fOnFormat) then
    fOnFormat(Self, pWaveFormat, FreeIt);
end;

function TLiveAudioPlayer.GetWaveData(const Buffer: Pointer;
  BufferSize: DWORD; var NumLoops: DWORD): DWORD;
begin
  if Assigned(fOnData) then
    Result := fOnData(Self, Buffer, BufferSize, NumLoops)
  else
    Result := 0;
end;

function TLiveAudioPlayer.GetWaveDataPtr(var Buffer: Pointer;
  var NumLoops: DWORD; var FreeIt: Boolean): DWORD;
begin
  if Assigned(fOnDataPtr) then
    Result := fOnDataPtr(Self, Buffer, NumLoops, FreeIt)
  else
    Result := 0;
end;

procedure TLiveAudioPlayer.SetPosition(Value: DWORD);
begin
  // Do Nothing
end;

function TLiveAudioPlayer.BreakLoop: Boolean;
begin
  Result := (waveOutBreakLoop(Handle) = MMSYSERR_NOERROR);
end;

{ TStockAudioPlayer }

procedure TStockAudioPlayer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Stock) then
    Stock := nil;
end;

procedure TStockAudioPlayer.SetStock(Value: TCustomWaveStorage);
begin
  if Stock <> Value then
  begin
    {$IFDEF COMPILER5_UP}
    if Assigned(Stock) then
      Stock.RemoveFreeNotification(Self);
    {$ENDIF}
    fStock := Value;
    if Assigned(Stock) then
      Stock.FreeNotification(Self);
  end;
end;

procedure TStockAudioPlayer.SetActive(Value: Boolean);
begin
  if Active <> Value then
  begin
    if Value then
      PlayStock(0)
    else
      Stop;
  end;
end;

procedure TStockAudioPlayer.SetPosition(Value: DWORD);
begin
  if Assigned(Wave) then
    Wave.Position := GetWaveDataPositionOffset(Wave.WaveFormat, Value);
  inherited SetPosition(Value);
end;

procedure TStockAudioPlayer.GetWaveFormat(var pWaveFormat: PWaveFormatEx;
  var FreeIt: Boolean);
begin
  FreeIt := False;
  if Wave.BeginRead then
  begin
    pWaveFormat := Wave.WaveFormat;
    Wave.Position := GetWaveDataPositionOffset(pWaveFormat, StartPosition);
  end;
end;

function TStockAudioPlayer.GetWaveData(const Buffer: Pointer;
  BufferSize: DWORD; var NumLoops: DWORD): DWORD;
begin
  Result := Wave.Read(Buffer^, BufferSize);
end;

procedure TStockAudioPlayer.DoWaveOutDeviceClose;
begin
  try
    Wave.EndRewrite;
    Wave.Free;
    Wave := nil;
  finally
    inherited DoWaveOutDeviceClose;
  end;
end;

procedure TStockAudioPlayer.DoError;
begin
  try
    if not Active and Assigned(Wave) then
    begin
      Wave.EndRewrite;
      Wave.Free;
      Wave := nil;
    end;
  finally
    inherited DoError;
  end;
end;

function TStockAudioPlayer.InternalPlay(AStream: TStream;
  Ownership: TStreamOwnership): Boolean;
begin
  Result := False;
  if Active then
  begin
    inherited Active := False;
    Sleep(0);
  end;
  Wave := TWaveStreamAdapter.Create(AStream, Ownership);
  try
    Result := InternalOpen;
  finally
    if (Result = False) and Assigned(Wave) then
    begin
      Wave.Free;
      Wave := nil;
    end;
  end;
end;

function TStockAudioPlayer.PlayStream(AStream: TStream): Boolean;
begin
  Result := InternalPlay(AStream, soReference);
end;

function TStockAudioPlayer.PlayFile(const FileName: String): Boolean;
begin
  Result := InternalPlay(TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite), soOwned);
end;

function TStockAudioPlayer.PlayResName(Instance: THandle;
  const ResName: String): Boolean;
begin
  Result := InternalPlay(TResourceStream.Create(Instance, ResName, 'WAVE'), soOwned);
end;

function TStockAudioPlayer.PlayResID(Instance: THandle; ResID: Integer): Boolean;
begin
  Result := InternalPlay(TResourceStream.CreateFromID(Instance, ResID, 'WAVE'), soOwned);
end;

function TStockAudioPlayer.PlayStock(Index: Integer): Boolean;
begin
  if not Assigned(Stock) then
    raise EWaveAudioInvalidOperation.Create('Stock property is not assigned');
  Result := InternalPlay(Stock.WaveStream[Index], soReference);
end;

function TStockAudioPlayer.Stop: Boolean;
begin
  Result := InternalClose;
end;

end.
