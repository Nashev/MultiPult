unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, mmSystem, WaveUtils, WaveStorage, StdCtrls, WaveIO, WaveOut,
  WavePlayers, ComCtrls, Buttons;

type
  TMainForm = class(TForm)
    StartPosLabel: TLabel;
    btnPlay: TBitBtn;
    btnPause: TBitBtn;
    btnStop: TBitBtn;
    Progress: TProgressBar;
    StockAudioPlayer: TStockAudioPlayer;
    btnSelectFile: TButton;
    WaveFileName: TEdit;
    AudioFormatLabel: TLabel;
    EndPosLabel: TLabel;
    OpenDialog: TOpenDialog;
    procedure btnSelectFileClick(Sender: TObject);
    procedure btnPlayClick(Sender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure StockAudioPlayerActivate(Sender: TObject);
    procedure StockAudioPlayerDeactivate(Sender: TObject);
    procedure StockAudioPlayerLevel(Sender: TObject; Level: Integer);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.btnSelectFileClick(Sender: TObject);
var
  WaveFile: TWaveFile;
begin
  if OpenDialog.Execute then
  begin
    WaveFileName.Text := OpenDialog.FileName;
    WaveFile := TWaveFile.Create(OpenDialog.FileName, fmOpenRead or fmShareDenyWrite);
    try
      AudioFormatLabel.Caption := WaveFile.AudioFormat;
      Progress.Position := 0;
      Progress.Max := WaveFile.Length;
      EndPosLabel.Caption := MS2Str(WaveFile.Length, msSh) + ' s';
    finally
      WaveFile.Free;
    end;
    btnPlay.Enabled := True;
  end;
end;

procedure TMainForm.btnPlayClick(Sender: TObject);
begin
  StockAudioPlayer.PlayFile(WaveFileName.Text);
end;

procedure TMainForm.btnPauseClick(Sender: TObject);
begin
  StockAudioPlayer.Paused := not StockAudioPlayer.Paused;
end;

procedure TMainForm.btnStopClick(Sender: TObject);
begin
  StockAudioPlayer.Stop;
end;

procedure TMainForm.StockAudioPlayerActivate(Sender: TObject);
begin
  btnPause.Enabled := True;
  btnStop.Enabled := True;
  btnPlay.Enabled := False;
end;

procedure TMainForm.StockAudioPlayerDeactivate(Sender: TObject);
begin
  btnPlay.Enabled := True;
  btnPause.Enabled := False;
  btnStop.Enabled := False;
end;

procedure TMainForm.StockAudioPlayerLevel(Sender: TObject; Level: Integer);
begin
  Progress.Position := StockAudioPlayer.Position;
end;

end.
