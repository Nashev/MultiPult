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
  ScreenFormUnit in 'ScreenFormUnit.pas' {ScreenForm},
  Direct3D9 in 'Webcam_Capture_V2\Common\DirectX\Direct3D9.pas',
  DirectDraw in 'Webcam_Capture_V2\Common\DirectX\DirectDraw.pas',
  DirectShow9 in 'Webcam_Capture_V2\Common\DirectX\DirectShow9.pas',
  DirectSound in 'Webcam_Capture_V2\Common\DirectX\DirectSound.pas',
  DXTypes in 'Webcam_Capture_V2\Common\DirectX\DXTypes.pas',
  VFrames in 'Webcam_Capture_V2\Common\VFrames.pas',
  VSample in 'Webcam_Capture_V2\Common\VSample.pas',
  CameraFormUnit in 'CameraFormUnit.pas' {CameraForm},
  ExportSizeCustomRequestDialogUnit in 'ExportSizeCustomRequestDialogUnit.pas' {ExportSizeCustomRequestDialog},
  WorkingSetManagementFormUnit in 'WorkingSetManagementFormUnit.pas' {WorkingSetManagementForm},
  MP3ConvertFormUnit in 'MP3ConvertFormUnit.pas' {MP3ConvertForm},
  MovieNameDialogUnit in 'MovieNameDialogUnit.pas' {MovieNameDialog},
  UtilsUnit in 'UtilsUnit.pas',
  DirMon in 'DirMon.pas';

{$R *.res}

resourcestring
  rs_AppCaption = 'ÌףכüעטÏףכüע';

begin
  Application.Initialize;
  Application.Title := rs_AppCaption;
  Application.HintHidePause := 8000;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TControllerForm, ControllerForm);
  Application.CreateForm(TScreenForm, ScreenForm);
  Application.CreateForm(TWorkingSetManagementForm, WorkingSetManagementForm);
  Application.CreateForm(TCameraForm, CameraForm);
  Application.Run;
end.
