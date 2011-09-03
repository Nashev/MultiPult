{------------------------------------------------------------------------------}
{                                                                              }
{  Wave Audio Package - Audio Redirector Demo                                  }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, WaveRedirector, mmSystem, WaveUtils,
  WaveStorage;

type

  TMainForm = class(TForm)
    Redirector: TAudioRedirector;
    ckActive: TCheckBox;
    AudioLevel: TProgressBar;
    Label1: TLabel;
    Format: TComboBox;
    SaveDialog: TSaveDialog;
    FormatLabel: TLabel;
    Label2: TLabel;
    ckSave: TCheckBox;
    FileNameLabel: TLabel;
    FileName: TEdit;
    btnBrowse: TButton;
    procedure ckActiveClick(Sender: TObject);
    procedure RedirectorActivate(Sender: TObject);
    procedure RedirectorDeactivate(Sender: TObject);
    procedure RedirectorAudioInLevel(Sender: TObject; Level: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormatChange(Sender: TObject);
    procedure ckSaveClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.ckActiveClick(Sender: TObject);
var
  Stream: TStream;
begin
  if ckActive.Checked and ckSave.Checked then
  begin
    Stream := TFileStream.Create(FileName.Text, fmCreate or fmShareExclusive);
    Redirector.Stream := Stream;
  end;
  Redirector.Active := ckActive.Checked;
  if not ckActive.Checked and ckSave.Checked then
  begin
    Stream := Redirector.Stream;
    Redirector.Stream := nil;
    Stream.Free;
  end;
end;

procedure TMainForm.RedirectorActivate(Sender: TObject);
begin
  ckActive.Checked := True;
  Format.Enabled := False;
  FileName.ReadOnly := True;
  btnBrowse.Enabled := False;
  ckSave.Enabled := False;
end;

procedure TMainForm.RedirectorDeactivate(Sender: TObject);
begin
  ckActive.Checked := False;
  Format.Enabled := True;
  FileName.ReadOnly := False;
  btnBrowse.Enabled := True;
  ckSave.Enabled := True;
end;

procedure TMainForm.RedirectorAudioInLevel(Sender: TObject; Level: Integer);
begin
  AudioLevel.Position := Level;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  pcm: TPCMFormat;
  WaveFormatEx: TWaveFormatEx;
begin
  for pcm := Succ(Low(TPCMFormat)) to High(TPCMFormat) do
  begin
    SetPCMAudioFormatS(@WaveFormatEx, pcm);
    Format.Items.Append(GetWaveAudioFormat(@WaveFormatEx));
  end;
  Format.ItemIndex := Ord(Redirector.AudioIn.PCMFormat) - 1;
  // Deplhi 6 and higher: the following event can be set at designtime
  Redirector.AudioIn.OnLevel := RedirectorAudioInLevel;
end;

procedure TMainForm.FormatChange(Sender: TObject);
begin
  Redirector.AudioIn.PCMFormat := TPCMFormat(Format.ItemIndex + 1);
end;

procedure TMainForm.ckSaveClick(Sender: TObject);
begin
  if ckSave.Checked and (FileName.Text = '') then
    btnBrowse.Click;
end;

procedure TMainForm.btnBrowseClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    FileName.Text := SaveDialog.FileName
  else if FileName.Text = '' then
    ckSave.Checked := False;
end;

end.
