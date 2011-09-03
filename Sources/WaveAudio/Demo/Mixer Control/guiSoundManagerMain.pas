unit guiSoundManagerMain;

interface

uses
  guiAudioDevice,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, WaveMixer, ExtCtrls, ComCtrls;

type
  TfrmSoundManagerMain = class(TForm)
    AudioMixer: TAudioMixer;
    MixersTreeView: TTreeView;
    Splitter: TSplitter;
    MixerControlsContainer: TScrollBox;
    procedure FormShow(Sender: TObject);
    procedure MixersTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure FormCreate(Sender: TObject);
    procedure MixersTreeViewDeletion(Sender: TObject; Node: TTreeNode);
  private
    SelectedDeviceForm: TfrmAudioDevice;
  end;

var
  frmSoundManagerMain: TfrmSoundManagerMain;

implementation

{$R *.dfm}

procedure TfrmSoundManagerMain.FormCreate(Sender: TObject);
var
  MixerID, DestID: Integer;
  DeviceForm: TfrmAudioDevice;
  DeviceNode: TTreeNode;
begin
  for MixerID := 0 to AudioMixer.MixerCount - 1 do
  begin
    AudioMixer.MixerID := MixerID;
    DeviceNode := MixersTreeView.Items.AddChild(nil, AudioMixer.MixerName);
    for DestID := 0 to AudioMixer.DestinationCount - 1 do
    begin
      AudioMixer.DestinationID := DestID;
      DeviceForm := TfrmAudioDevice.CreateDevice(MixerControlsContainer, MixerID, DestID);
      MixersTreeView.Items.AddChildObject(DeviceNode, AudioMixer.DestinationName, DeviceForm);
    end;
    DeviceNode.Expand(False);
  end;
end;

procedure TfrmSoundManagerMain.FormShow(Sender: TObject);
begin
  if (MixersTreeView.Selected = nil) and (MixersTreeView.Items.Count > 0) then
      MixersTreeView.Selected := MixersTreeView.TopItem.Item[0];
end;

procedure TfrmSoundManagerMain.MixersTreeViewChange(Sender: TObject;
  Node: TTreeNode);
begin
  if Assigned(SelectedDeviceForm) then
  begin
    SelectedDeviceForm.Hide;
    SelectedDeviceForm := nil;
  end;
  if Assigned(Node.Data) then
  begin
    SelectedDeviceForm := TfrmAudioDevice(Node.Data);
    SelectedDeviceForm.Show;
  end;
end;

procedure TfrmSoundManagerMain.MixersTreeViewDeletion(Sender: TObject;
  Node: TTreeNode);
begin
  if Assigned(Node.Data) then
  begin
    TfrmAudioDevice(Node.Data).Free;
    Node.Data := nil;
  end;
end;

end.
