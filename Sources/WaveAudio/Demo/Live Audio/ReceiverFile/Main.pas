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
  Spin, StdCtrls, Buttons, mmSystem, WaveUtils, WaveStorage, WaveIO, WaveOut,
  WavePlayers, ComCtrls, ScktComp;

const
  WM_RECEIVERREADY = WM_USER;

type
  TMainForm = class(TForm)
    btnDisconnect: TButton;
    btnConnect: TButton;
    gbBroadcaster: TGroupBox;
    lblRemoteAddress: TLabel;
    lblRemotePort: TLabel;
    edRemoteAddress: TEdit;
    seRemotePort: TSpinEdit;
    edFormat: TEdit;
    lblFormat: TLabel;
    SaveDialog: TSaveDialog;
    lblFileSize: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tcpClientConnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure tcpClientDisconnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure tcpClientRead(Sender: TObject; Socket: TCustomWinSocket);
  private
    tcpClient: TClientSocket;
    WaveFile: TWaveFile;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  WinSock;

{ Helper Functions }

function FormatSize(Size: Int64): String;
const
  KB = 1024;
  MB = 1024 * KB;
  GB = 1024 * MB;
begin
  if Size < KB then
    Result := FormatFloat('#,##0 Bytes', Size)
  else if Size < MB then
    Result := FormatFloat('#,##0.0 KB', Size / KB)
  else if Size < GB then
    Result := FormatFloat('#,##0.0 MB', Size / MB)
  else
    Result := FormatFloat('#,##0.0 GB', Size / GB);
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
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  tcpClient.Active := False;
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
  btnConnect.Visible := True;
  btnDisconnect.Visible := False;
  edRemoteAddress.Enabled := True;
  seRemotePort.Enabled := True;
  edFormat.Text := '';
  if Assigned(WaveFile) then
  begin
    WaveFile.EndRewrite;
    WaveFile.Free;
    WaveFile := nil;
    lblFileSize.Caption := '';
  end;
end;

procedure TMainForm.btnConnectClick(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    WaveFile := TWaveFile.Create(SaveDialog.FileName, fmCreate or fmShareDenyWrite);
    tcpClient.Host := edRemoteAddress.Text;
    tcpClient.Port := seRemotePort.Value;
    tcpClient.Active := True;
  end;
end;

procedure TMainForm.btnDisconnectClick(Sender: TObject);
begin
  tcpClient.Active := False;
end;

procedure TMainForm.tcpClientRead(Sender: TObject; Socket: TCustomWinSocket);
var
  WaveFormat: PWaveFormatEx;
  WaveFormatSize: Integer;
  Data: Pointer;
  DataSize: Integer;
begin
  try
    if WaveFile.State = wssReady then // Getting Wave Format
    begin
      Socket.ReceiveBuf(WaveFormatSize, SizeOf(WaveFormatSize));
      GetMem(WaveFormat, WaveFormatSize);
      try
        Socket.ReceiveBuf(WaveFormat^, WaveFormatSize);
        edFormat.Text := GetWaveAudioFormat(WaveFormat);
        WaveFile.BeginRewrite(WaveFormat);
        Socket.SendText('READY');
        lblFileSize.Caption := 'Waiting for audio data...';
      finally
        FreeMem(WaveFormat);
      end;
    end
    else
    begin
      Sleep(0);
      DataSize := Socket.ReceiveLength;
      if DataSize > 0 then
      begin
        GetMem(Data, DataSize);
        try
          Socket.ReceiveBuf(Data^, DataSize);
          WaveFile.Write(Data^, DataSize);
          lblFileSize.Caption := 'Receiving: ' + FormatSize(WaveFile.Stream.Size);
        finally
          FreeMem(Data);
        end;
      end;
    end;
  except
    tcpClient.Active := False;
    Application.HandleException(Self);
  end;
end;

end.
