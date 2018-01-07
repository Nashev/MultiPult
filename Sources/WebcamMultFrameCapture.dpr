program WebcamMultFrameCapture;

uses
  Vcl.Forms,
  WaveACM in 'WaveAudio\WaveACM.pas',
  WaveTimer in 'WaveAudio\WaveTimer.pas',
  WaveUtils in 'WaveAudio\WaveUtils.pas',
  DXTypes in 'Webcam_Capture_V2\Common\DirectX\DXTypes.pas',
  VFrames in 'Webcam_Capture_V2\Common\VFrames.pas',
  VSample in 'Webcam_Capture_V2\Common\VSample.pas',
  Direct3D9 in 'Webcam_Capture_V2\Common\DirectX\Direct3D9.pas',
  DirectDraw in 'Webcam_Capture_V2\Common\DirectX\DirectDraw.pas',
  DirectShow9 in 'Webcam_Capture_V2\Common\DirectX\DirectShow9.pas',
  DirectSound in 'Webcam_Capture_V2\Common\DirectX\DirectSound.pas',
  CameraFormUnit in 'CameraFormUnit.pas' {CameraForm},
  ScreenFormUnit in 'ScreenFormUnit.pas' {ScreenForm},
  DirMon in 'DirMon.pas',
  UtilsUnit in 'UtilsUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TScreenForm, ScreenForm);
  Application.CreateForm(TCameraForm, CameraForm);
  ScreenForm.imgCamPreview.Show;
  ScreenForm.OnControlButtonClick := CameraForm.ToggleVisibility;
  ScreenForm.OnKeyPress := CameraForm.FormKeyPress;

  CameraForm.imgOverlay := ScreenForm.imgOverlay;
  CameraForm.imgCamPreview := ScreenForm.imgCamPreview;
  CameraForm.OnOpacityChanged := ScreenForm.AdjustOpacity;
  //CameraForm.OnCloseQuery := ScreenForm.DoCloseQueryScreenForm;
  //CameraForm.OnClose := ScreenForm.DoCloseScreenForm;
  CameraForm.Active := True;

  ScreenForm.Caption :=  CameraForm.Caption;
  Application.Title := CameraForm.Caption;

  // на случай, если что-то пойдёт не так
  VersionNameString := '0.9.???';
  VersionCopyrightString := 'МультиСтудия, Москва, 20??';
  TakeVersionInfo;

  Application.Run;
end.
