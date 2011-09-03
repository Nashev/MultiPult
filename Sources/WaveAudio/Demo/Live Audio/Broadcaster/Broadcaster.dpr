program Broadcaster;

uses
  Forms,
  Main in 'Main.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Audio Broadcaster (Server)';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
