unit ProgressFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ACS_Classes, ACS_WinMedia, ACS_smpeg, StdCtrls,
  ACS_Wave, Vcl.ExtCtrls;

type
  TProgressForm = class(TForm)
    ProgressBar1: TProgressBar;
    lblCaption: TLabel;
    lblCaption2: TLabel;
    lblCaption3: TLabel;
    btnCancel: TButton;
    procedure btnCancelClick(Sender: TObject);
  private
    FOnCancel: TNotifyEvent;
  public
    constructor Create(const ACaption, ACaption2, ACaption3: string; AOnCancel: TNotifyEvent); reintroduce; virtual;
    procedure SetProgress(AValue, AMaxValue: Integer);
    procedure SetProgressStatus(const ACaption3: string);
  end;

var
  ProgressForm: TProgressForm;

implementation

{$R *.dfm}

procedure TProgressForm.btnCancelClick(Sender: TObject);
begin
  FOnCancel(Self);
end;

constructor TProgressForm.Create(const ACaption, ACaption2, ACaption3: string; AOnCancel: TNotifyEvent);
begin
  inherited Create(Application);
  Caption := Application.Title;
  lblCaption .Caption := ACaption;
  lblCaption2.Caption := ACaption2;
  lblCaption3.Caption := ACaption3;
  FOnCancel := AOnCancel;
end;

procedure TProgressForm.SetProgress(AValue, AMaxValue: Integer);
begin
  ProgressBar1.Max := AMaxValue;
  ProgressBar1.Position := AValue;
end;

procedure TProgressForm.SetProgressStatus(const ACaption3: string);
begin
  lblCaption3.Caption := ACaption3;
end;

end.
