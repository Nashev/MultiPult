unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, mmSystem, WaveUtils, WaveStorage, WaveIO, WaveIn, WaveRecorders,
  WaveOut, WavePlayers, StdCtrls, ComCtrls;

type
  TMainForm = class(TForm)
    btnRecord: TButton;
    ProgressBar: TProgressBar;
    LiveRecorder: TLiveAudioRecorder;
    TargetFormat: TWaveStorage;
    btnStop: TButton;
    btnPlay: TButton;
    LivePlayer: TLiveAudioPlayer;
    Label1: TLabel;
    cbFormat: TComboBox;
    Label2: TLabel;
    ebFileFormat: TEdit;
    btnTargetFormat: TButton;
    OpenDialog: TOpenDialog;
    procedure btnRecordClick(Sender: TObject);
    procedure LiveRecorderData(Sender: TObject; const Buffer: Pointer;
      BufferSize: Cardinal; var FreeIt: Boolean);
    procedure LiveRecorderActivate(Sender: TObject);
    procedure LiveRecorderDeactivate(Sender: TObject);
    procedure LiveRecorderLevel(Sender: TObject; Level: Integer);
    procedure btnStopClick(Sender: TObject);
    procedure LivePlayerActivate(Sender: TObject);
    procedure LivePlayerDeactivate(Sender: TObject);
    function LivePlayerData(Sender: TObject; const Buffer: Pointer;
      BufferSize: Cardinal; var NumLoops: Cardinal): Cardinal;
    procedure LivePlayerLevel(Sender: TObject; Level: Integer);
    procedure btnPlayClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbFormatChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnTargetFormatClick(Sender: TObject);
  private
    WaveFile: TWaveFileConverter;
    procedure UpdateControls;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

const
  OutputFileName = 'Output.wav';

procedure TMainForm.UpdateControls;
begin
  cbFormat.Enabled := not LiveRecorder.Active and not LivePlayer.Active;
  btnRecord.Enabled := not LiveRecorder.Active and not LivePlayer.Active
    and WaveFile.CanRewrite(TargetFormat.Wave.WaveFormat);
  btnStop.Enabled := LiveRecorder.Active or LivePlayer.Active;
  btnPlay.Enabled := not LiveRecorder.Active and not LivePlayer.Active
    and WaveFile.CanRead;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  Mode: Word;
  pcm: TPCMFormat;
  WaveFormatEx: TWaveFormatEx;
begin
  for pcm := Succ(Low(TPCMFormat)) to High(TPCMFormat) do
  begin
    SetPCMAudioFormatS(@WaveFormatEx, pcm);
    cbFormat.Items.Append(GetWaveAudioFormat(@WaveFormatEx));
  end;
  cbFormat.ItemIndex := Ord(LiveRecorder.PCMFormat) - 1;
  Mode := fmOpenReadWrite or fmShareDenyWrite;
  if not FileExists(OutputFileName) then
    Mode := Mode or fmCreate;
  WaveFile := TWaveFileConverter.Create(OutputFileName, Mode);
  WaveFile.SetBufferFormatPCM(LiveRecorder.PCMFormat);
  ebFileFormat.Text := TargetFormat.Wave.AudioFormat;
  UpdateControls;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  WaveFile.Free;
end;

procedure TMainForm.btnTargetFormatClick(Sender: TObject);
var
  TemplateWaveFile: TWaveFile;
begin
  if OpenDialog.Execute then
  begin
    TemplateWaveFile := TWaveFile.Create(OpenDialog.FileName, fmOpenRead or fmShareDenyWrite);
    try
      TargetFormat.Wave.BeginRewrite(TemplateWaveFile.WaveFormat);
      TargetFormat.Wave.EndRewrite;
    finally
      TemplateWaveFile.Free;
    end;
    ebFileFormat.Text := TargetFormat.Wave.AudioFormat;
    UpdateControls;
  end;
end;

procedure TMainForm.cbFormatChange(Sender: TObject);
begin
  LiveRecorder.PCMFormat := TPCMFormat(cbFormat.ItemIndex + 1);
  LivePlayer.PCMFormat := TPCMFormat(cbFormat.ItemIndex + 1);
  WaveFile.SetBufferFormatPCM(TPCMFormat(cbFormat.ItemIndex + 1));
  UpdateControls;
end;

procedure TMainForm.btnRecordClick(Sender: TObject);
begin
  LiveRecorder.Active := True;
end;

procedure TMainForm.btnStopClick(Sender: TObject);
begin
  LiveRecorder.Active := False;
  LivePlayer.Active := False;
end;

procedure TMainForm.btnPlayClick(Sender: TObject);
begin
  LivePlayer.Active := True;
end;

procedure TMainForm.LiveRecorderActivate(Sender: TObject);
begin
  WaveFile.BeginRewrite(TargetFormat.Wave.WaveFormat);
  UpdateControls;
end;

procedure TMainForm.LiveRecorderData(Sender: TObject; const Buffer: Pointer;
  BufferSize: Cardinal; var FreeIt: Boolean);
begin
  WaveFile.Write(Buffer^,BufferSize);
end;

procedure TMainForm.LiveRecorderDeactivate(Sender: TObject);
begin
  WaveFile.EndRewrite;
  UpdateControls;
end;

procedure TMainForm.LiveRecorderLevel(Sender: TObject; Level: Integer);
begin
  ProgressBar.Position := Level;
end;

procedure TMainForm.LivePlayerActivate(Sender: TObject);
begin
  WaveFile.BeginRead;
  UpdateControls;
end;

function TMainForm.LivePlayerData(Sender: TObject; const Buffer: Pointer;
  BufferSize: Cardinal; var NumLoops: Cardinal): Cardinal;
begin
  Result := WaveFile.Read(Buffer^, BufferSize);
end;

procedure TMainForm.LivePlayerDeactivate(Sender: TObject);
begin
  WaveFile.EndRead;
  UpdateControls;
end;

procedure TMainForm.LivePlayerLevel(Sender: TObject; Level: Integer);
begin
  ProgressBar.Position := Level;
end;

end.
