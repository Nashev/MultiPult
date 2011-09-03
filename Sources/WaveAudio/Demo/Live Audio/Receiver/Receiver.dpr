program Receiver;

uses
  Forms,
  Main in 'Main.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Audio Receiver (Client)';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
