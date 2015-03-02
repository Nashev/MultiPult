program Webcam;

{$R 'WebcamResource.res' 'WebcamResource.rc'}

uses
  Vcl.Forms,
  VFrames in 'Common\VFrames.pas',
  VSample in 'Common\VSample.pas',
  Direct3D9 in 'Common\DirectX\Direct3D9.pas',
  DirectDraw in 'Common\DirectX\DirectDraw.pas',
  DirectShow9 in 'Common\DirectX\DirectShow9.pas',
  DirectSound in 'Common\DirectX\DirectSound.pas',
  DXTypes in 'Common\DirectX\DXTypes.pas',
  Vcl.Themes,
  Vcl.Styles,
  camform in 'camform.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Webcam Capture';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
