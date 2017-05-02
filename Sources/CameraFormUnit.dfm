object CameraForm: TCameraForm
  Left = 461
  Top = 110
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  Caption = #1055#1086#1083#1091#1095#1077#1085#1080#1077' '#1082#1072#1076#1088#1086#1074' '#1089' '#1074#1077#1073'-'#1082#1072#1084#1077#1088#1099
  ClientHeight = 421
  ClientWidth = 528
  Color = clBtnFace
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
  OnShow = FormShow
  DesignSize = (
    528
    421)
  PixelsPerInch = 96
  TextHeight = 13
  object imgPreview: TImage
    Left = 16
    Top = 63
    Width = 496
    Height = 296
    Anchors = [akLeft, akTop, akRight, akBottom]
    Center = True
    Proportional = True
    Stretch = True
    ExplicitWidth = 499
  end
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
    Left = 450
    Top = 392
    Width = 3
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = ' '
    ExplicitTop = 398
  end
  object lblFolder: TLabel
    Left = 16
    Top = 397
    Width = 43
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = #1042' '#1087#1072#1087#1082#1091':'
  end
  object btnMakePhoto: TButton
    Left = 14
    Top = 365
    Width = 113
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = #1042#1079#1103#1090#1100' '#1086#1076#1080#1085' '#1082#1072#1076#1088
    TabOrder = 0
    OnClick = btnMakePhotoClick
  end
  object cbCamSelector: TComboBox
    Left = 64
    Top = 8
    Width = 348
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = #1050#1072#1084#1077#1088#1072
    OnChange = cbCamSelectorChange
  end
  object btnNextCam: TButton
    Left = 460
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
    Width = 348
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    Text = '640'#1093'480'
    OnChange = cbbResolutionChange
  end
  object btnPreferences: TButton
    Left = 418
    Top = 35
    Width = 94
    Height = 25
    Anchors = [akTop, akRight]
    Caption = #1055#1072#1088#1072#1084#1077#1090#1088#1099
    TabOrder = 4
    OnClick = btnPreferencesClick
  end
  object btnTimeLapse: TButton
    Left = 154
    Top = 365
    Width = 154
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = #1041#1088#1072#1090#1100' '#1082#1072#1076#1088' '#1082#1072#1078#1076#1099#1077
    TabOrder = 5
    OnClick = btnTimeLapseClick
  end
  object seInterval: TSpinEdit
    Left = 314
    Top = 367
    Width = 62
    Height = 22
    Anchors = [akLeft, akBottom]
    MaxValue = 100000
    MinValue = 1
    TabOrder = 6
    Value = 1
  end
  object cbbUnit: TComboBox
    Left = 382
    Top = 367
    Width = 59
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 7
    Text = #1089#1077#1082
    Items.Strings = (
      #1084#1089#1077#1082
      #1089#1077#1082)
  end
  object edtFolder: TEdit
    Left = 65
    Top = 397
    Width = 447
    Height = 19
    Anchors = [akLeft, akRight, akBottom]
    BevelInner = bvNone
    BorderStyle = bsNone
    Color = clBtnFace
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 8
    Text = 'edtFolder'
  end
  object btnFolderLookup: TButton
    Left = 487
    Top = 391
    Width = 25
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '...'
    TabOrder = 9
    Visible = False
    OnClick = btnFolderLookupClick
  end
  object btnStart: TButton
    Left = 418
    Top = 8
    Width = 36
    Height = 25
    Anchors = [akTop, akRight]
    Caption = #1055#1091#1089#1082
    TabOrder = 10
    OnClick = btnStartClick
  end
  object TimeLapseTimer: TMultimediaTimer
    OnTimer = TimeLapseTimerTimer
    Left = 195
    Top = 329
  end
  object TimeLapseStatusTimer: TMultimediaTimer
    Interval = 200
    Resolution = 200
    OnTimer = TimeLapseStatusTimerTimer
    Left = 228
    Top = 328
  end
end
