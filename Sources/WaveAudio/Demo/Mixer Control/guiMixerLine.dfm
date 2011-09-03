object frmMixerLine: TfrmMixerLine
  Left = 373
  Top = 193
  Align = alLeft
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'frmMixerLine'
  ClientHeight = 245
  ClientWidth = 108
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Visible = True
  DesignSize = (
    108
    245)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 107
    Top = 0
    Width = 1
    Height = 245
    Align = alRight
    Shape = bsLeftLine
  end
  object lblName: TLabel
    Left = 0
    Top = 0
    Width = 107
    Height = 30
    Alignment = taCenter
    AutoSize = False
    Caption = 'lblName'
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    ShowAccelChar = False
    Transparent = False
    Layout = tlCenter
    WordWrap = True
  end
  object Bevel2: TBevel
    Left = 0
    Top = 29
    Width = 107
    Height = 2
    Anchors = [akLeft, akTop, akRight]
    Shape = bsBottomLine
  end
  object trbVolume: TTrackBar
    Left = 40
    Top = 33
    Width = 27
    Height = 184
    Anchors = [akTop, akBottom]
    Max = 100
    Orientation = trVertical
    PageSize = 5
    Frequency = 10
    Position = 50
    TabOrder = 0
    ThumbLength = 10
    TickMarks = tmBoth
    Visible = False
    OnChange = trbVolumeChange
  end
  object cbxMute: TCheckBox
    Left = 32
    Top = 220
    Width = 50
    Height = 17
    Anchors = [akBottom]
    Caption = 'Mute'
    TabOrder = 1
    Visible = False
    OnClick = cbxMuteClick
  end
  object cbxSelect: TCheckBox
    Left = 32
    Top = 220
    Width = 48
    Height = 17
    Anchors = [akBottom]
    Caption = 'Select'
    TabOrder = 2
    Visible = False
    OnClick = cbxSelectClick
  end
end
