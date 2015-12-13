object ControllerForm: TControllerForm
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsToolWindow
  Caption = #1059#1087#1088#1072#1074#1083#1077#1085#1080#1077
  ClientHeight = 318
  ClientWidth = 325
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  OnActivate = FormActivate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object pbScrollHandle: TPaintBox
    Left = 0
    Top = 0
    Width = 325
    Height = 318
    Hint = 
      #1044#1074#1080#1075#1072#1081#1090#1077' '#1084#1099#1096#1100' ('#1080#1083#1080' '#1087#1072#1083#1077#1094', '#1077#1089#1083#1080' '#1101#1082#1088#1072#1085' '#1089#1077#1085#1089#1086#1088#1085#1099#1081') '#1087#1086' '#1082#1088#1091#1075#1091' '#13#10#1076#1083#1103' '#1087 +
      #1077#1088#1077#1093#1086#1076#1072' '#1086#1090' '#1082#1072#1076#1088#1072' '#1082' '#1082#1072#1076#1088#1091', '#1087#1088#1086#1082#1088#1091#1095#1080#1074#1072#1085#1080#1103' '#1083#1077#1085#1090#1099' '#1082#1072#1076#1088#1086#1074'.'#13#10#1055#1088#1080' '#1089#1083#1091#1095#1072 +
      #1081#1085#1099#1093' '#1087#1088#1080#1073#1083#1080#1078#1077#1085#1080#1103#1093' '#1084#1099#1096#1080' '#1082' '#1094#1077#1085#1090#1088#1091', '#1076#1077#1083#1072#1074#1096#1080#1077#1089#1103' '#1087#1077#1088#1077#1093#1086#1076#1099' '#13#10#1087#1088#1086#1076#1086#1083#1078#1072#1102 +
      #1090#1089#1103' '#1072#1074#1090#1086#1084#1072#1090#1080#1095#1077#1089#1082#1080', '#1087#1086#1076#1076#1077#1088#1078#1080#1074#1072#1103' '#1087#1088#1077#1078#1085#1102#1102' '#1089#1082#1086#1088#1086#1089#1090#1100'.'
    Align = alClient
    OnMouseEnter = pbScrollHandleMouseEnter
    OnMouseLeave = pbScrollHandleMouseLeave
    OnMouseMove = pbScrollHandleMouseMove
    OnPaint = pbScrollHandlePaint
  end
  object lblFramerate: TLabel
    Left = 8
    Top = 8
    Width = 46
    Height = 13
    Hint = #1057#1082#1086#1088#1086#1089#1090#1100' '#1087#1077#1088#1077#1082#1083#1102#1095#1077#1085#1080#1103' '#1082#1072#1076#1088#1086#1074', '#1074' '#1089#1077#1082#1091#1085#1076#1091
    Caption = '### / 25'
  end
  object tmrAutoMode: TTimer
    Enabled = False
    OnTimer = tmrAutoModeTimer
    Left = 64
    Top = 32
  end
end
