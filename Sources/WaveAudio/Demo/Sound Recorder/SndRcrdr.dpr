program SndRcrdr;

uses
  Forms,
  Main in 'Main.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Sound Recorder';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
