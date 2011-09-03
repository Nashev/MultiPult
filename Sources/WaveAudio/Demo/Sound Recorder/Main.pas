{------------------------------------------------------------------------------}
{                                                                              }
{  Wave Audio Package - Recorder / Player Demo                                 }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, WaveIn, WaveRecorders, WaveIO, WaveOut,
  WavePlayers, StdCtrls, Buttons, ExtCtrls, mmSystem, WaveUtils,
  WaveStorage;

type
  TMainForm = class(TForm)
    btnSelectFile: TButton;
    btnRecord: TBitBtn;
    btnPlay: TBitBtn;
    btnPause: TBitBtn;
    btnStop: TBitBtn;
    WaveFile: TLabel;
    AudioLevel: TProgressBar;
    Position: TLabel;
    SaveDialog: TSaveDialog;
    StockAudioPlayer: TStockAudioPlayer;
    StockAudioRecorder: TStockAudioRecorder;
    PositionLabel: TLabel;
    Format: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormatChange(Sender: TObject);
    procedure btnSelectFileClick(Sender: TObject);
    procedure btnRecordClick(Sender: TObject);
    procedure btnPlayClick(Sender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure StockAudioPlayerActivate(Sender: TObject);
    procedure StockAudioPlayerDeactivate(Sender: TObject);
    procedure StockAudioPlayerLevel(Sender: TObject; Level: Integer);
    procedure StockAudioRecorderActivate(Sender: TObject);
    procedure StockAudioRecorderDeactivate(Sender: TObject);
    procedure StockAudioRecorderLevel(Sender: TObject; Level: Integer);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
var
  pcm: TPCMFormat;
  WaveFormatEx: TWaveFormatEx;
begin
  for pcm := Succ(Low(TPCMFormat)) to High(TPCMFormat) do
  begin
    SetPCMAudioFormatS(@WaveFormatEx, pcm);
    Format.Items.Append(GetWaveAudioFormat(@WaveFormatEx));
  end;
  Format.ItemIndex := Ord(StockAudioRecorder.PCMFormat) - 1;
  WaveFile.Caption := ChangeFileExt(ExtractFilePath(Application.ExeName) +
    SaveDialog.FileName, '.' + SaveDialog.DefaultExt);
  btnRecord.Enabled := True;
  btnPlay.Enabled := FileExists(WaveFile.Caption);
end;

procedure TMainForm.FormatChange(Sender: TObject);
begin
  StockAudioRecorder.PCMFormat := TPCMFormat(Format.ItemIndex + 1);
end;

procedure TMainForm.btnSelectFileClick(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    WaveFile.Caption := SaveDialog.FileName;
    btnRecord.Enabled := True;
    btnPlay.Enabled := FileExists(SaveDialog.FileName);
  end;
end;

procedure TMainForm.btnRecordClick(Sender: TObject);
begin
  StockAudioRecorder.RecordToFile(WaveFile.Caption);
end;

procedure TMainForm.btnPlayClick(Sender: TObject);
begin
  StockAudioPlayer.PlayFile(WaveFile.Caption);
end;

procedure TMainForm.btnPauseClick(Sender: TObject);
begin
  if StockAudioPlayer.Active then
    StockAudioPlayer.Paused := not StockAudioPlayer.Paused
  else
    StockAudioRecorder.Paused := not StockAudioRecorder.Paused;
end;

procedure TMainForm.btnStopClick(Sender: TObject);
begin
  if StockAudioPlayer.Active then
    StockAudioPlayer.Active := False
  else
    StockAudioRecorder.Active := False;
end;

procedure TMainForm.StockAudioPlayerActivate(Sender: TObject);
begin
  Format.Enabled := False;
  btnSelectFile.Enabled := False;
  btnPause.Enabled := True;
  btnStop.Enabled := True;
  btnRecord.Enabled := False;
  btnPlay.Enabled := False;
  btnStop.SetFocus;
end;

procedure TMainForm.StockAudioPlayerDeactivate(Sender: TObject);
begin
  Format.Enabled := True;
  btnSelectFile.Enabled := True;
  btnRecord.Enabled := True;
  btnPlay.Enabled := True;
  btnPause.Enabled := False;
  btnStop.Enabled := False;
  btnPlay.SetFocus;
end;

procedure TMainForm.StockAudioPlayerLevel(Sender: TObject; Level: Integer);
begin
  Position.Caption := MS2Str(StockAudioPlayer.Position, msAh);
  AudioLevel.Position := Level;
end;

procedure TMainForm.StockAudioRecorderActivate(Sender: TObject);
begin
  Format.Enabled := False;
  btnSelectFile.Enabled := False;
  btnPause.Enabled := True;
  btnStop.Enabled := True;
  btnRecord.Enabled := False;
  btnPlay.Enabled := False;
  btnStop.SetFocus;
end;

procedure TMainForm.StockAudioRecorderDeactivate(Sender: TObject);
begin
  Format.Enabled := True;
  btnSelectFile.Enabled := True;
  btnRecord.Enabled := True;
  btnPlay.Enabled := True;
  btnPause.Enabled := False;
  btnStop.Enabled := False;
  btnPlay.SetFocus;
  Position.Caption := '0.00';
end;

procedure TMainForm.StockAudioRecorderLevel(Sender: TObject;
  Level: Integer);
begin
  Position.Caption := MS2Str(StockAudioRecorder.Position, msAh);
  AudioLevel.Position := Level;
end;

end.
