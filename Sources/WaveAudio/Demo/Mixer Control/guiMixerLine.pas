unit guiMixerLine;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, WaveMixer, StdCtrls, ComCtrls;

type
  TfrmMixerLine = class(TForm)
    trbVolume: TTrackBar;
    Bevel1: TBevel;
    cbxMute: TCheckBox;
    cbxSelect: TCheckBox;
    lblName: TLabel;
    Bevel2: TBevel;
    procedure cbxSelectClick(Sender: TObject);
    procedure cbxMuteClick(Sender: TObject);
    procedure trbVolumeChange(Sender: TObject);
  private
    fMixerLine: TAudioMixerLine;
  public
    constructor CreateLine(AParent: TWinControl; AMixerLine: TAudioMixerLine);
    procedure UpdateControls(ControlTypes: TMixerControlTypes);
    property MixerLine: TAudioMixerLine read fMixerLine;
  end;

implementation

{$R *.dfm}

constructor TfrmMixerLine.CreateLine(AParent: TWinControl;
  AMixerLine: TAudioMixerLine);
begin
  inherited Create(nil);
  Parent := AParent;
  Top := 0;
  Left := AMixerLine.ID * Width;
  fMixerLine := AMixerLine;
  lblName.Caption := MixerLine.Name;
  if MixerLine.Mixer.Master = MixerLine then
  begin
    lblName.Color := $00CCCCFF;
    cbxSelect.Visible := False;
  end
  else if (MixerLine.Mixer.Master.AvailableControls * [mcSelect, mcMix]) <> [] then
    cbxSelect.Visible := True
  else
    cbxSelect.Visible := False;
  trbVolume.Visible := mcVolume in MixerLine.AvailableControls;
  cbxMute.Visible := mcMute in MixerLine.AvailableControls;
  UpdateControls([mcVolume, mcMute, mcSelect, mcMix]);
end;

procedure TfrmMixerLine.UpdateControls(ControlTypes: TMixerControlTypes);
begin
  if mcVolume in ControlTypes then
    trbVolume.Position := 100 - MixerLine.Volume;
  if McMute in ControlTypes then
    cbxMute.Checked := MixerLine.Mute;
  if (mcSelect in ControlTypes) or (mcMix in ControlTypes) then
    cbxSelect.Checked := (MixerLine.Mixer.Master.SelectedLine = MixerLine.ID);
end;

procedure TfrmMixerLine.trbVolumeChange(Sender: TObject);
begin
  MixerLine.Volume := 100 - trbVolume.Position;
end;

procedure TfrmMixerLine.cbxMuteClick(Sender: TObject);
begin
  MixerLine.Mute := cbxMute.Checked;
end;

procedure TfrmMixerLine.cbxSelectClick(Sender: TObject);
begin
  if cbxSelect.Checked then
    MixerLine.Mixer.Master.SelectedLine := MixerLine.ID
  else if cbxSelect.Focused then
    cbxSelect.Checked := True;
end;


end.
