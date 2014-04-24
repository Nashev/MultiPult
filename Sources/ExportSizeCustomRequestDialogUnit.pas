unit ExportSizeCustomRequestDialogUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin;

type
  TExportSizeCustomRequestDialog = class(TForm)
    seWidth: TSpinEdit;
    seHeight: TSpinEdit;
    lblCaption: TLabel;
    btnOk: TButton;
    btnCancel: TButton;
    chkKeepAspectRate: TCheckBox;
    procedure seKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure chkKeepAspectRateClick(Sender: TObject);
    procedure seWidthChange(Sender: TObject);
  private
  public
    InitialSize: TSize;
    class function Execute(var ASize: TSize): Boolean;
  end;

var
  ExportSizeCustomRequestDialog: TExportSizeCustomRequestDialog;

implementation

uses
  MainFormUnit;

{$R *.dfm}

{ TExportSizeCustomRequestDialog }

procedure TExportSizeCustomRequestDialog.chkKeepAspectRateClick(
  Sender: TObject);
begin
  seHeight.ReadOnly := chkKeepAspectRate.Checked;
  if seHeight.ReadOnly then
    begin
      seHeight.Color := clBtnFace;
      InitialSize.cx := seWidth.Value;
      InitialSize.cy := seHeight.Value;
    end
  else
    seHeight.Color := clWindow;
end;

class function TExportSizeCustomRequestDialog.Execute(
  var ASize: TSize): Boolean;
begin
  Result := False;
  with Create(Application) do
    try
      Caption := Application.Title;
      InitialSize := ASize;
      seWidth.Value := ASize.cx;
      seHeight.Value := ASize.cy;
      if ShowModal = mrOk then
        begin
          ASize.cx := seWidth.Value;
          ASize.cy := seHeight.Value;
          Result := True;
        end;
    finally
      Free;
    end;
end;

procedure TExportSizeCustomRequestDialog.seKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  // само у кнопок по Default и Cancel выславленным в True не работает. Видимо, SpinEdit перехватывает (
  if (Key = VK_RETURN) then
    ModalResult := mrOk
  else if (Key = VK_ESCAPE) then
    ModalResult := mrCancel;
end;

procedure TExportSizeCustomRequestDialog.seWidthChange(Sender: TObject);
begin
  if chkKeepAspectRate.Checked then
    seHeight.Value := MulDiv(seWidth.Value, InitialSize.cy, InitialSize.cx);
end;

end.
