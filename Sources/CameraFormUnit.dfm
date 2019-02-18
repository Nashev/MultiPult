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
  ShowHint = True
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
    Width = 3
    Height = 13
    Caption = ' '
  end
  object lblFolder: TLabel
    Left = 17
    Top = 97
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
  object seInterval: TSpinEdit
    Left = 435
    Top = 69
    Width = 62
    Height = 22
    MaxValue = 100000
    MinValue = 1
    TabOrder = 6
    Value = 1
  end
  object cbbUnit: TComboBox
    Left = 503
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
    Top = 97
    Width = 568
    Height = 19
    Anchors = [akLeft, akTop, akRight]
    BevelInner = bvNone
    BorderStyle = bsNone
    Color = clBtnFace
    Ctl3D = False
    ParentCtl3D = False
    ReadOnly = True
    TabOrder = 8
    Text = 'edtFolder'
    OnDblClick = btnFolderLookupClick
  end
  object btnFolderLookup: TButton
    Left = 640
    Top = 91
    Width = 25
    Height = 25
    Hint = #1042#1099#1073#1088#1072#1090#1100' '#1087#1072#1087#1082#1091'...'
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
  object edtOverlay: TComboBox
    Left = 114
    Top = 122
    Width = 450
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    Ctl3D = False
    ItemIndex = 0
    ParentCtl3D = False
    TabOrder = 11
    Text = 'pathToOverlay'
    OnChange = btnReloadOverlayClick
    OnDblClick = btnSelectOverlayClick
    Items.Strings = (
      'pathToOverlay')
  end
  object btnSelectOverlay: TButton
    Left = 570
    Top = 120
    Width = 25
    Height = 25
    Hint = #1042#1099#1073#1088#1072#1090#1100' '#1092#1072#1081#1083' '#1080#1079#1086#1073#1088#1072#1078#1077#1085#1080#1103' '#1076#1083#1103' '#1088#1072#1079#1084#1077#1090#1082#1080' '#1082#1072#1076#1088#1072'...'
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 12
    OnClick = btnSelectOverlayClick
  end
  object chkOverlay: TCheckBox
    Left = 10
    Top = 123
    Width = 98
    Height = 17
    Caption = #1056#1072#1079#1084#1077#1090#1082#1072' '#1082#1072#1076#1088#1072':'
    TabOrder = 13
    OnClick = chkOverlayClick
  end
  object chkMinimize: TCheckBox
    Left = 118
    Top = 71
    Width = 139
    Height = 17
    Caption = #1048' '#1089#1074#1077#1088#1085#1091#1090#1100' '#1087#1088#1086#1075#1088#1072#1084#1084#1091
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
    Left = 593
    Top = 120
    Width = 25
    Height = 25
    Hint = #1055#1077#1088#1077#1095#1080#1090#1072#1090#1100' '#1092#1072#1081#1083' '#1088#1072#1079#1084#1077#1090#1082#1080' '#1082#1072#1076#1088#1072
    Anchors = [akTop, akRight]
    Caption = #8634
    Enabled = False
    TabOrder = 15
    OnClick = btnReloadOverlayClick
  end
  object btnTimeLapse: TButton
    Left = 281
    Top = 67
    Width = 148
    Height = 25
    Caption = #1041#1088#1072#1090#1100' '#1082#1072#1076#1088' '#1082#1072#1078#1076#1099#1077
    TabOrder = 5
    OnClick = btnTimeLapseClick
  end
  object btnNextOverlay: TButton
    Left = 640
    Top = 120
    Width = 25
    Height = 25
    Hint = #1057#1083#1077#1076#1091#1102#1097#1080#1081' '#1092#1072#1081#1083' '#1088#1072#1079#1084#1077#1090#1082#1080' '#1082#1072#1076#1088#1072
    Anchors = [akTop, akRight]
    Caption = '>'
    Enabled = False
    TabOrder = 17
    OnClick = btnNextOverlayClick
  end
  object btnPrevOverlay: TButton
    Left = 617
    Top = 120
    Width = 25
    Height = 25
    Hint = #1055#1088#1077#1076#1099#1076#1091#1097#1080#1081' '#1092#1072#1081#1083' '#1088#1072#1079#1084#1077#1090#1082#1080' '#1082#1072#1076#1088#1072
    Anchors = [akTop, akRight]
    Caption = '<'
    Enabled = False
    TabOrder = 18
    OnClick = btnPrevOverlayClick
  end
  object chkAutoChangeOverlay: TCheckBox
    Left = 32
    Top = 146
    Width = 632
    Height = 17
    Caption = #1052#1077#1085#1103#1090#1100' '#1092#1072#1081#1083' '#1088#1072#1079#1084#1077#1090#1082#1080' '#1085#1072' '#1089#1083#1077#1076#1091#1102#1097#1080#1081' '#1087#1088#1080' '#1082#1072#1078#1076#1086#1084' '#1074#1079#1103#1090#1080#1080' '#1082#1072#1076#1088#1072
    TabOrder = 19
    OnClick = chkOverlayClick
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
