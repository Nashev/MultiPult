object CameraForm: TCameraForm
  Left = 607
  Top = 352
  Caption = #1055#1086#1083#1091#1095#1077#1085#1080#1077' '#1082#1072#1076#1088#1086#1074' '#1089' '#1074#1077#1073'-'#1082#1072#1084#1077#1088#1099
  ClientHeight = 427
  ClientWidth = 465
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
    465
    427)
  PixelsPerInch = 96
  TextHeight = 13
  object imgPreview: TImage
    Left = 16
    Top = 63
    Width = 433
    Height = 325
    Anchors = [akLeft, akTop, akRight, akBottom]
    Center = True
    Proportional = True
    Stretch = True
    ExplicitWidth = 406
    ExplicitHeight = 285
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
  object btnMakePhoto: TButton
    Left = 164
    Top = 394
    Width = 121
    Height = 25
    Anchors = [akLeft, akRight, akBottom]
    Caption = #1042#1079#1103#1090#1100' '#1082#1072#1076#1088
    TabOrder = 0
    OnClick = btnMakePhotoClick
  end
  object cbCamSelector: TComboBox
    Left = 64
    Top = 8
    Width = 285
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = #1050#1072#1084#1077#1088#1072
    OnChange = cbCamSelectorChange
  end
  object btnNextCam: TButton
    Left = 355
    Top = 8
    Width = 94
    Height = 25
    Anchors = [akTop, akRight]
    Caption = #1057#1083#1077#1076#1091#1102#1097#1072#1103
    TabOrder = 2
    OnClick = btnNextCamClick
  end
  object cbbResolution: TComboBox
    Left = 64
    Top = 36
    Width = 285
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    Text = '640'#1093'480'
    OnChange = cbbResolutionChange
  end
  object btnPreferences: TButton
    Left = 355
    Top = 35
    Width = 94
    Height = 25
    Anchors = [akTop, akRight]
    Caption = #1055#1072#1088#1072#1084#1077#1090#1088#1099
    TabOrder = 4
    OnClick = btnPreferencesClick
  end
end
