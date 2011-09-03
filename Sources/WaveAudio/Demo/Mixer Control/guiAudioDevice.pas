unit guiAudioDevice;

interface

uses
  guiMixerLine,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, WaveMixer;

type
  TfrmAudioDevice = class(TForm)
    AudioMixer: TAudioMixer;
    procedure AudioMixerControlChange(Sender: TObject;
      MixerLine: TAudioMixerLine; ControlType: TMixerControlType);
  private
    MasterForm: TfrmMixerLine;
    LineForms: TList;
  public
    constructor CreateDevice(AParent: TWinControl;
      MixerID, DestinationID: Integer);
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

constructor TfrmAudioDevice.CreateDevice(AParent: TWinControl;
  MixerID, DestinationID: Integer);
var
  I: Integer;
begin
  inherited Create(nil);
  Parent := AParent;
  Left := 0;
  Top := 0;
  AudioMixer.MixerID := MixerID;
  AudioMixer.DestinationID := DestinationID;
  if ((AudioMixer.Master.AvailableControls * [mcSelect, mcMix]) = []) and
     ((AudioMixer.Master.AvailableControls * [mcVolume, mcMute]) <> []) then
  begin
    MasterForm := TfrmMixerLine.CreateLine(Self, AudioMixer.Master);
  end;
  LineForms := TList.Create;
  for I := 0 to AudioMixer.LineCount - 1 do
    LineForms.Add(TfrmMixerLine.CreateLine(Self, AudioMixer.Lines[I]));
end;

destructor TfrmAudioDevice.Destroy;
var
  I: Integer;
begin
  if Assigned(MasterForm) then
    MasterForm.Free;
  for I := 0 to LineForms.Count - 1 do
    TfrmMixerLine(LineForms[I]).Free;
  LineForms.Free;
  inherited Destroy;
end;

procedure TfrmAudioDevice.AudioMixerControlChange(Sender: TObject;
  MixerLine: TAudioMixerLine; ControlType: TMixerControlType);
var
  I: Integer;
begin
  if ControlType in [mcSelect, mcMix] then
  begin
    // update controls of all lines (mcSelect and mcMux are mutual switches)
    for I := 0 to LineForms.Count - 1 do
      TfrmMixerLine(LineForms[I]).UpdateControls([ControlType]);
  end
  else if Assigned(MasterForm) and (MasterForm.MixerLine = MixerLine) then
  begin
    // update controls of master line
    MasterForm.UpdateControls([ControlType]);
  end
  else
  begin
    // update controls of the source line
    for I := 0 to LineForms.Count - 1 do
      if TfrmMixerLine(LineForms[I]).MixerLine = MixerLine then
      begin
        TfrmMixerLine(LineForms[I]).UpdateControls([ControlType]);
        Break;
      end;
  end;
end;

end.
