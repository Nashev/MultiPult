program MultPult;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
{$IFNDEF FPC}
{$ELSE}
  Interfaces,
{$ENDIF}
  Forms,
  Gauges in 'Gauges.pas',
  MainFormUnit in 'MainFormUnit.pas' {MainForm},
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
//  AVIReading in 'avi_work\AVIReading.pas',
  vfw in 'avi_work\vfw.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
