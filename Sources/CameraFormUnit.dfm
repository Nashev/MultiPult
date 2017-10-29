object CameraForm: TCameraForm
  Left = 460
  Top = 183
  AlphaBlend = True
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsToolWindow
  Caption = #1055#1086#1083#1091#1095#1077#1085#1080#1077' '#1082#1072#1076#1088#1086#1074' '#1089' '#1074#1077#1073'-'#1082#1072#1084#1077#1088#1099
  ClientHeight = 208
  ClientWidth = 680
  Color = clBtnFace
  TransparentColorValue = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnHide = FormHide
  OnKeyPress = FormKeyPress
  OnResize = FormResize
  DesignSize = (
    680
    208)
  PixelsPerInch = 96
  TextHeight = 13
  object lblCamSelector: TLabel
    Left = 17
    Top = 11
    Width = 41
    Height = 13
    Caption = #1050#1072#1084#1077#1088#1072':'
  end
  object lblResolution: TLabel
    Left = 17
    Top = 39
    Width = 39
    Height = 13
    Caption = #1056#1072#1079#1084#1077#1088':'
  end
  object lblLapseStatus: TLabel
    Left = 17
    Top = 101
    Width = 647
    Height = 13
    Caption = ' '
  end
  object lblFolder: TLabel
    Left = 17
    Top = 123
    Width = 43
    Height = 13
    Caption = #1042' '#1087#1072#1087#1082#1091':'
  end
  object lblOpacity: TLabel
    Left = 17
    Top = 181
    Width = 158
    Height = 13
    Caption = #1055#1088#1086#1079#1088#1072#1095#1085#1086#1089#1090#1100' '#1074#1080#1076#1077#1086' '#1089' '#1082#1072#1084#1077#1088#1099':'
  end
  object btnMakePhoto: TButton
    Left = 14
    Top = 67
    Width = 98
    Height = 25
    Caption = #1042#1079#1103#1090#1100' '#1086#1076#1080#1085' '#1082#1072#1076#1088
    TabOrder = 0
    OnClick = btnMakePhotoClick
  end
  object cbCamSelector: TComboBox
    Left = 64
    Top = 8
    Width = 500
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = #1050#1072#1084#1077#1088#1072
    OnChange = cbCamSelectorChange
  end
  object btnNextCam: TButton
    Left = 612
    Top = 8
    Width = 52
    Height = 25
    Anchors = [akTop, akRight]
    Caption = #1044#1088#1091#1075#1072#1103
    TabOrder = 2
    OnClick = btnNextCamClick
  end
  object cbbResolution: TComboBox
    Left = 64
    Top = 36
    Width = 500
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    Text = '640'#1093'480'
    OnChange = cbbResolutionChange
  end
  object btnPreferences: TButton
    Left = 570
    Top = 35
    Width = 94
    Height = 25
    Anchors = [akTop, akRight]
    Caption = #1055#1072#1088#1072#1084#1077#1090#1088#1099
    TabOrder = 4
    OnClick = btnPreferencesClick
  end
  object btnTimeLapse: TButton
    Left = 248
    Top = 67
    Width = 148
    Height = 25
    Caption = #1041#1088#1072#1090#1100' '#1082#1072#1076#1088' '#1082#1072#1078#1076#1099#1077
    TabOrder = 5
    OnClick = btnTimeLapseClick
  end
  object seInterval: TSpinEdit
    Left = 402
    Top = 69
    Width = 62
    Height = 22
    MaxValue = 100000
    MinValue = 1
    TabOrder = 6
    Value = 1
  end
  object cbbUnit: TComboBox
    Left = 470
    Top = 69
    Width = 59
    Height = 21
    ItemIndex = 1
    TabOrder = 7
    Text = #1089#1077#1082
    Items.Strings = (
      #1084#1089#1077#1082
      #1089#1077#1082)
  end
  object edtFolder: TEdit
    Left = 66
    Top = 123
    Width = 568
    Height = 19
    Anchors = [akLeft, akTop, akRight]
    BevelInner = bvNone
    BorderStyle = bsNone
    Color = clBtnFace
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 8
    Text = 'edtFolder'
    OnDblClick = btnFolderLookupClick
  end
  object btnFolderLookup: TButton
    Left = 640
    Top = 117
    Width = 25
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 9
    OnClick = btnFolderLookupClick
  end
  object btnStart: TButton
    Left = 570
    Top = 8
    Width = 36
    Height = 25
    Anchors = [akTop, akRight]
    Caption = #1055#1091#1089#1082
    TabOrder = 10
    OnClick = btnStartClick
  end
  object edtOverlay: TEdit
    Left = 114
    Top = 150
    Width = 497
    Height = 19
    Anchors = [akLeft, akTop, akRight]
    BevelInner = bvNone
    BorderStyle = bsNone
    Color = clBtnFace
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 11
    Text = 'edtOverlay'
    OnDblClick = btnSelectOverlayClick
  end
  object btnSelectOverlay: TButton
    Left = 617
    Top = 148
    Width = 25
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 12
    OnClick = btnSelectOverlayClick
  end
  object chkOverlay: TCheckBox
    Left = 15
    Top = 148
    Width = 98
    Height = 17
    Caption = #1042#1080#1076#1086#1080#1089#1082#1072#1090#1077#1083#1100':'
    TabOrder = 13
    OnClick = chkOverlayClick
  end
  object chkMinimize: TCheckBox
    Left = 118
    Top = 71
    Width = 124
    Height = 17
    Caption = #1048' '#1089#1082#1088#1099#1090#1100' '#1101#1090#1086' '#1086#1082#1085#1086
    TabOrder = 16
  end
  object tbOpacity: TTrackBar
    Left = 181
    Top = 174
    Width = 484
    Height = 45
    Anchors = [akLeft, akTop, akRight]
    LineSize = 8
    Max = 255
    Min = 31
    PageSize = 32
    Position = 31
    ShowSelRange = False
    TabOrder = 14
    OnChange = tbOpacityChange
  end
  object btnReloadOverlay: TButton
    Left = 640
    Top = 148
    Width = 25
    Height = 25
    Anchors = [akTop, akRight]
    Caption = #8634
    TabOrder = 15
    OnClick = btnReloadOverlayClick
  end
  object TimeLapseTimer: TMultimediaTimer
    OnTimer = TimeLapseTimerTimer
    Left = 195
    Top = 4
  end
  object TimeLapseStatusTimer: TMultimediaTimer
    Interval = 200
    Resolution = 200
    OnTimer = TimeLapseStatusTimerTimer
    Left = 228
    Top = 3
  end
end
