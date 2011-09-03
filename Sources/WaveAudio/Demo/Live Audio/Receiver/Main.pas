{------------------------------------------------------------------------------}
{                                                                              }
{  Wave Audio Package - Audio Broadcasting Demo (Client)                       }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Spin, StdCtrls, Buttons, mmSystem, WaveUtils, WaveIO, WaveOut, WavePlayers,
  ComCtrls, ScktComp, WaveStorage;

type

  TAudioBuffer = class
  private
    CS: RTL_CRITICAL_SECTION;
    Data: Pointer;
    Size: Cardinal;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function BeginUpdate(ExtraSize: Cardinal): Pointer;
    procedure EndUpdate;
    function Get(var Buffer: Pointer; var BufferSize: Cardinal): Boolean;
  end;

  TMainForm = class(TForm)
    btnDisconnect: TButton;
    btnConnect: TButton;
    LiveAudioPlayer: TLiveAudioPlayer;
    gbBroadcaster: TGroupBox;
    lblRemoteAddress: TLabel;
    lblRemotePort: TLabel;
    edRemoteAddress: TEdit;
    seRemotePort: TSpinEdit;
    pbLevel: TProgressBar;
    edFormat: TEdit;
    lblFormat: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tcpClientConnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure tcpClientDisconnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure LiveAudioPlayerActivate(Sender: TObject);
    procedure LiveAudioPlayerDeactivate(Sender: TObject);
    procedure LiveAudioPlayerError(Sender: TObject);
    procedure LiveAudioPlayerLevel(Sender: TObject; Level: Integer);
    procedure LiveAudioPlayerFormat(Sender: TObject;
      var pWaveFormat: PWaveFormatEx; var FreeIt: Boolean);
    function LiveAudioPlayerDataPtr(Sender: TObject; var Buffer: Pointer;
      var NumLoops: Cardinal; var FreeIt: Boolean): Cardinal;
    procedure tcpClientRead(Sender: TObject; Socket: TCustomWinSocket);
  private
    tcpClient: TClientSocket;
    AudioBuffer: TAudioBuffer;
    WaveFormat: PWaveFormatEx;
    BlockAlign: Integer;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  WinSock;

{ TAudioBuffers }

constructor TAudioBuffer.Create;
begin
  InitializeCriticalSection(CS);
end;

destructor TAudioBuffer.Destroy;
begin
  Clear;
  DeleteCriticalSection(CS);
  inherited Destroy;
end;

procedure TAudioBuffer.Clear;
begin
  EnterCriticalSection(CS);
  try
    ReallocMem(Data, 0);
    Size := 0;
  finally
    LeaveCriticalSection(CS);
  end;
end;

function TAudioBuffer.BeginUpdate(ExtraSize: Cardinal): Pointer;
begin
  EnterCriticalSection(CS);
  ReallocMem(Data, Size + ExtraSize);
  Result := Pointer(Cardinal(Data) + Size);
  Inc(Size, ExtraSize);
end;

procedure TAudioBuffer.EndUpdate;
begin
  LeaveCriticalSection(CS);
end;

function TAudioBuffer.Get(var Buffer: Pointer; var BufferSize: Cardinal): Boolean;
begin
  EnterCriticalSection(CS);
  try
    Result := False;
    if Assigned(Data) then
    begin
      Buffer := Data;
      BufferSize := Size;
      Data := nil;
      Size := 0;
      Result := True;
    end;
  finally
    LeaveCriticalSection(CS);
  end;
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  tcpClient := TClientSocket.Create(Self);
  with tcpClient do
  begin
    ClientType := ctNonBlocking;
    OnConnect := tcpClientConnect;
    OnDisconnect := tcpClientDisconnect;
    OnRead := tcpClientRead;
  end;
  AudioBuffer := TAudioBuffer.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  tcpClient.Active := False;
  LiveAudioPlayer.WaitForStop;
  AudioBuffer.Free;
end;

procedure TMainForm.tcpClientConnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  btnDisconnect.Visible := True;
  btnConnect.Visible := False;
  edRemoteAddress.Enabled := False;
  seRemotePort.Enabled := False;
end;

procedure TMainForm.tcpClientDisconnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  AudioBuffer.Clear;
  LiveAudioPlayer.Active := False;
  btnConnect.Visible := True;
  btnDisconnect.Visible := False;
  edRemoteAddress.Enabled := True;
  seRemotePort.Enabled := True;
  edFormat.Text := '';
end;

procedure TMainForm.btnConnectClick(Sender: TObject);
begin
  tcpClient.Host := edRemoteAddress.Text;
  tcpClient.Port := seRemotePort.Value;
  tcpClient.Active := True;
end;

procedure TMainForm.btnDisconnectClick(Sender: TObject);
begin
  tcpClient.Active := False;
end;

{$IFDEF UNICODE}
// Delphi 2009 BUG:
// Socket.SendText writes AnsiString but Socket.ReceiveText reads WideString!
// Therefore we implement our own SendText to be sure things are symmetrical.
procedure TMainForm.LiveAudioPlayerActivate(Sender: TObject);
var
  Text: AnsiString;
begin
  Text := 'READY';
  tcpClient.Socket.SendBuf(PAnsiChar(Text)^, Length(Text));
end;
{$ELSE}
procedure TMainForm.LiveAudioPlayerActivate(Sender: TObject);
begin
  tcpClient.Socket.SendText('READY');
end;
{$ENDIF}

procedure TMainForm.LiveAudioPlayerDeactivate(Sender: TObject);
begin
  tcpClient.Active := False;
end;

procedure TMainForm.LiveAudioPlayerError(Sender: TObject);
begin
  tcpClient.Active := False;
  MessageDlg(LiveAudioPlayer.LastErrorText, mtError, [mbOK], 0);
end;

procedure TMainForm.LiveAudioPlayerLevel(Sender: TObject; Level: Integer);
begin
  pbLevel.Position := Level;
end;

procedure TMainForm.LiveAudioPlayerFormat(Sender: TObject;
  var pWaveFormat: PWaveFormatEx; var FreeIt: Boolean);
begin
  FreeIt := True;
  pWaveFormat := WaveFormat;
  edFormat.Text := GetWaveAudioFormat(WaveFormat);
  BlockAlign := WaveFormat^.nBlockAlign;
  WaveFormat := nil;
end;

function TMainForm.LiveAudioPlayerDataPtr(Sender: TObject;
  var Buffer: Pointer; var NumLoops: DWORD;
  var FreeIt: Boolean): DWORD;
begin
  if not tcpClient.Active then
    Result := 0    // Stops LiveAudioPlayer
  else if AudioBuffer.Get(Buffer, Result) then
    FreeIt := True
  else
  begin
    Buffer := nil; // When Buffer is nil,
    Result := 10   // Result will be considered as silence milliseconds.
  end
end;

procedure TMainForm.tcpClientRead(Sender: TObject; Socket: TCustomWinSocket);
var
  WaveFormatSize: Integer;
  Data: Pointer;
  DataSize: Integer;
begin
  try
    if not LiveAudioPlayer.Active then
    begin
      Socket.ReceiveBuf(WaveFormatSize, SizeOf(WaveFormatSize));
      ReallocMem(WaveFormat, WaveFormatSize);
      try
        Socket.ReceiveBuf(WaveFormat^, WaveFormatSize);
      except
        ReallocMem(WaveFormat, 0);
        raise;
      end;
      LiveAudioPlayer.Active := True;
    end
    else
    begin
      Sleep(0); // giving chance to the player thread for using the audio buffer
      DataSize := Socket.ReceiveLength;
      if BlockAlign > 1 then
        Dec(DataSize, DataSize mod BlockAlign);
      if DataSize <> 0 then
      begin
        Data := AudioBuffer.BeginUpdate(DataSize);
        try
          Socket.ReceiveBuf(Data^, DataSize);
        finally
          AudioBuffer.EndUpdate;
        end;
      end;
    end;
  except
    tcpClient.Active := False;
    Application.HandleException(Self);
  end;
end;

end.
