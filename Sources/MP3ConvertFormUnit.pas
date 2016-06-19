unit MP3ConvertFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ACS_Classes, ACS_WinMedia, ACS_smpeg, StdCtrls,
  ACS_Wave, Vcl.ExtCtrls;

type
  TMP3ConvertForm = class(TForm)
    MP3In1: TMP3In;
    ProgressBar1: TProgressBar;
    WaveOut1: TWaveOut;
    lblStstus: TLabel;
    procedure WaveOut1Done(Sender: TComponent);
    procedure WaveOut1Progress(Sender: TComponent);
    procedure FormShow(Sender: TObject);
    procedure WaveOut1ThreadException(Sender: TComponent);
  private
    { Private declarations }
  public
    class function Execute(AMP3FileName, AWavFileName: string): Boolean;
  end;

var
  MP3ConvertForm: TMP3ConvertForm;

implementation

{$R *.dfm}

class function TMP3ConvertForm.Execute(AMP3FileName, AWavFileName: string): Boolean;
begin
  with Create(Application) do
    try
      MP3In1.FileName := AMP3FileName;
      WaveOut1.FileName := AWavFileName;
      Result := ShowModal = mrOk;
    finally
      Free;
    end;
end;

procedure TMP3ConvertForm.FormShow(Sender: TObject);
begin
  WaveOut1.Run;
end;

procedure TMP3ConvertForm.WaveOut1Done(Sender: TComponent);
begin
  ModalResult := mrOk;
end;

procedure TMP3ConvertForm.WaveOut1Progress(Sender: TComponent);
begin
  ProgressBar1.Position := WaveOut1.Progress;
end;

procedure TMP3ConvertForm.WaveOut1ThreadException(Sender: TComponent);
begin
  ModalResult := mrCancel;
end;

end.
