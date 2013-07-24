program MultiPult;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  Forms,
  Gauges in 'Gauges.pas',
  MainFormUnit in 'MainFormUnit.pas' {MainForm},
  ControllerFormUnit in 'ControllerFormUnit.pas' {ControllerForm},
  WaveACM in 'WaveAudio\WaveACM.pas',
  WaveIn in 'WaveAudio\WaveIn.pas',
  WaveIO in 'WaveAudio\WaveIO.pas',
  WaveMixer in 'WaveAudio\WaveMixer.pas',
  WaveOut in 'WaveAudio\WaveOut.pas',
  WavePlayers in 'WaveAudio\WavePlayers.pas',
  WaveRecorders in 'WaveAudio\WaveRecorders.pas',
  WaveRedirector in 'WaveAudio\WaveRedirector.pas',
  WaveStorage in 'WaveAudio\WaveStorage.pas',
  WaveTimer in 'WaveAudio\WaveTimer.pas',
  WaveUtils in 'WaveAudio\WaveUtils.pas',
  AVICompression in 'avi_work\AVICompression.pas',
  AVIFile32 in 'avi_work\AVIFile32.pas',
  ScreenFormUnit in 'ScreenFormUnit.pas' {ScreenForm};

{$R *.res}

resourcestring
  rs_AppCaption = 'ÌףכüעטÏףכüע';

begin
  Application.Initialize;
  Application.Title := rs_AppCaption;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TControllerForm, ControllerForm);
  Application.CreateForm(TScreenForm, ScreenForm);
  Application.Run;
end.
