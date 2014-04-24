object ExportSizeCustomRequestDialog: TExportSizeCustomRequestDialog
  Left = 472
  Top = 261
  BorderStyle = bsDialog
  Caption = 'ExportSizeCustomRequestDialog'
  ClientHeight = 124
  ClientWidth = 329
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblCaption: TLabel
    Left = 12
    Top = 8
    Width = 180
    Height = 13
    Caption = #1056#1072#1079#1084#1077#1088#1099' '#1082#1072#1076#1088#1072' '#1087#1088#1080' '#1101#1082#1089#1087#1086#1088#1090#1077' '#1074' AVI:'
  end
  object seWidth: TSpinEdit
    Left = 16
    Top = 28
    Width = 121
    Height = 22
    MaxValue = 20000
    MinValue = 1
    TabOrder = 0
    Value = 1
    OnChange = seWidthChange
    OnKeyDown = seKeyDown
  end
  object seHeight: TSpinEdit
    Left = 156
    Top = 28
    Width = 121
    Height = 22
    Color = clBtnFace
    MaxValue = 20000
    MinValue = 1
    ReadOnly = True
    TabOrder = 1
    Value = 1
    OnKeyDown = seKeyDown
  end
  object btnOk: TButton
    Left = 110
    Top = 91
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object btnCancel: TButton
    Left = 202
    Top = 91
    Width = 75
    Height = 25
    Cancel = True
    Caption = #1054#1090#1084#1077#1085#1072
    ModalResult = 2
    TabOrder = 3
  end
  object chkKeepAspectRate: TCheckBox
    Left = 16
    Top = 64
    Width = 261
    Height = 17
    Caption = #1057#1086#1093#1088#1072#1085#1103#1090#1100' '#1087#1088#1086#1087#1088#1086#1094#1080#1080
    Checked = True
    State = cbChecked
    TabOrder = 4
    OnClick = chkKeepAspectRateClick
  end
end
