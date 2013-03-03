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
  OnActivate = FormActivate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object pbScrollHandle: TPaintBox
    Left = 0
    Top = 0
    Width = 325
    Height = 318
    Align = alClient
    OnMouseEnter = pbScrollHandleMouseEnter
    OnMouseLeave = pbScrollHandleMouseLeave
    OnMouseMove = pbScrollHandleMouseMove
    OnPaint = pbScrollHandlePaint
    ExplicitLeft = 128
    ExplicitTop = 112
    ExplicitWidth = 105
    ExplicitHeight = 105
  end
  object lblFramerate: TLabel
    Left = 8
    Top = 8
    Width = 46
    Height = 13
    Caption = '### / 25'
  end
  object tmrAutoMode: TTimer
    Enabled = False
    OnTimer = tmrAutoModeTimer
    Left = 64
    Top = 32
  end
end
