object frmSoundManagerMain: TfrmSoundManagerMain
  Left = 326
  Top = 415
  AutoScroll = False
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Sound Manager'
  ClientHeight = 214
  ClientWidth = 676
  Color = clBtnFace
  Constraints.MaxHeight = 250
  Constraints.MinHeight = 250
  Constraints.MinWidth = 200
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter: TSplitter
    Left = 184
    Top = 0
    Height = 214
    Beveled = True
    ResizeStyle = rsUpdate
  end
  object MixersTreeView: TTreeView
    Left = 0
    Top = 0
    Width = 184
    Height = 214
    Align = alLeft
    HideSelection = False
    Indent = 19
    ReadOnly = True
    TabOrder = 0
    OnChange = MixersTreeViewChange
    OnDeletion = MixersTreeViewDeletion
  end
  object MixerControlsContainer: TScrollBox
    Left = 187
    Top = 0
    Width = 489
    Height = 214
    Align = alClient
    TabOrder = 1
  end
  object AudioMixer: TAudioMixer
    Left = 80
    Top = 24
  end
end
