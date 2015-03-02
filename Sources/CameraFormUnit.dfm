object CameraForm: TCameraForm
  Left = 0
  Top = 0
  Caption = 'Webcam Capture'
  ClientHeight = 349
  ClientWidth = 701
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
    701
    349)
  PixelsPerInch = 96
  TextHeight = 13
  object imgPreview: TImage
    Left = 16
    Top = 35
    Width = 406
    Height = 275
    Anchors = [akLeft, akTop, akRight, akBottom]
    Center = True
    Proportional = True
    Stretch = True
  end
  object lblCamSelector: TLabel
    Left = 17
    Top = 11
    Width = 41
    Height = 13
    Caption = #1050#1072#1084#1077#1088#1072':'
  end
  object btnMakePhoto: TButton
    Left = 164
    Top = 316
    Width = 94
    Height = 25
    Anchors = [akLeft, akRight, akBottom]
    Caption = #1042#1079#1103#1090#1100' '#1082#1072#1076#1088
    TabOrder = 0
    OnClick = btnMakePhotoClick
  end
  object cbCamSelector: TComboBox
    Left = 64
    Top = 8
    Width = 258
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = #1050#1072#1084#1077#1088#1072
  end
  object btnNextCam: TButton
    Left = 328
    Top = 8
    Width = 94
    Height = 25
    Anchors = [akTop, akRight]
    Caption = #1057#1083#1077#1076#1091#1102#1097#1072#1103
    TabOrder = 2
    OnClick = btnNextCamClick
  end
end
