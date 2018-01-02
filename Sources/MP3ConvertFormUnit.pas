unit MP3ConvertFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ACS_Classes, ACS_WinMedia, ACS_smpeg, StdCtrls,
  ACS_Wave, Vcl.ExtCtrls, ProgressFormUnit;

type
  TMP3ConvertForm = class(TProgressForm)
    MP3In1: TMP3In;
    WaveOut1: TWaveOut;
    procedure WaveOut1Done(Sender: TComponent);
    procedure WaveOut1Progress(Sender: TComponent);
    procedure FormShow(Sender: TObject);
    procedure WaveOut1ThreadException(Sender: TComponent);
    procedure btnCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    class function Execute(AMP3FileName, AWavFileName: string): Boolean;
  end;

var
  MP3ConvertForm: TMP3ConvertForm;

implementation

{$R *.dfm}

procedure TMP3ConvertForm.btnCancelClick(Sender: TObject);
begin
  WaveOut1.Abort;
  DeleteFile(WaveOut1.FileName);
//  ModalResult := mrCancel;
end;

class function TMP3ConvertForm.Execute(AMP3FileName, AWavFileName: string): Boolean;
resourcestring
   rs_MP3toWAVconvert = 'Конвертация выбранного MP3 в WAV в папку с фотографиями:';
begin
  with Create(rs_MP3toWAVconvert, AMP3FileName, AWavFileName, nil) do
    try
      MP3In1.FileName := AMP3FileName;
      WaveOut1.FileName := AWavFileName;
      btnCancel.Hide; // TODO: resolve deadlock
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
