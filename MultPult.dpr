program MultPult;

uses
  Forms,
  MainFormUnit in 'MainFormUnit.pas' {MainForm},
  AVICompression in 'Downloads\avi_work\AVICompression.pas',
  AVIFile32 in 'Downloads\avi_work\AVIFile32.pas',
  vfw in 'Downloads\avi_work\vfw.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
