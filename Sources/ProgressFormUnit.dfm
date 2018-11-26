object ProgressForm: TProgressForm
  Left = 192
  Top = 114
  BorderIcons = []
  BorderWidth = 8
  Caption = 'Progress'
  ClientHeight = 95
  ClientWidth = 616
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    616
    95)
  PixelsPerInch = 96
  TextHeight = 13
  object lblCaption: TLabel
    Left = 0
    Top = 0
    Width = 616
    Height = 13
    Align = alTop
    Caption = 'Caption'
  end
  object lblCaption2: TLabel
    Left = 0
    Top = 13
    Width = 616
    Height = 13
    Align = alTop
    Caption = 'lblCaption2'
  end
  object lblCaption3: TLabel
    Left = 0
    Top = 26
    Width = 616
    Height = 13
    Align = alTop
    Caption = 'lblCaption3'
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 39
    Width = 616
    Height = 17
    Align = alTop
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 271
    Top = 62
    Width = 75
    Height = 25
    Anchors = [akTop]
    Cancel = True
    Caption = #1054#1090#1084#1077#1085#1080#1090#1100
    ModalResult = 2
    TabOrder = 1
    OnClick = btnCancelClick
  end
end
