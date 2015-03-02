unit WorkingSetManagementFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TWorkingSetManagementForm = class(TForm)
    lstFiles: TListBox;
  private
    { Private declarations }
  public
    procedure Execute;
  end;

var
  WorkingSetManagementForm: TWorkingSetManagementForm;

implementation

{$R *.dfm}

{ TWorkingSetManagementForm }

procedure TWorkingSetManagementForm.Execute;
begin
  ShowModal;
end;

end.
