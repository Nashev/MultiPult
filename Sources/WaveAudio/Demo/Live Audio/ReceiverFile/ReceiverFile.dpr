program ReceiverFile;

uses
  Forms,
  Main in 'Main.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Audio File Receiver (Client)';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
