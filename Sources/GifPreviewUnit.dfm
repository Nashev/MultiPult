object GifPreviewForm: TGifPreviewForm
  Left = 0
  Top = 0
  Caption = #1069#1082#1089#1087#1086#1088#1090' '#1074' GIF'
  ClientHeight = 666
  ClientWidth = 939
  Color = clBtnFace
  Constraints.MinHeight = 680
  Constraints.MinWidth = 650
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  DesignSize = (
    939
    666)
  PixelsPerInch = 96
  TextHeight = 13
  object pbPalettePreview: TPaintBox
    Left = 8
    Top = 503
    Width = 320
    Height = 80
    OnPaint = pbPalettePreviewPaint
  end
  object imgPreview: TImage
    Left = 334
    Top = 36
    Width = 596
    Height = 547
    Anchors = [akLeft, akTop, akRight, akBottom]
    Center = True
    Proportional = True
    Stretch = True
  end
  object lblSize: TLabel
    Left = 337
    Top = 8
    Width = 19
    Height = 13
    Caption = 'Size'
  end
  object lblSizeX: TLabel
    Left = 437
    Top = 8
    Width = 6
    Height = 13
    Caption = 'x'
  end
  object lblSizePX: TLabel
    Left = 520
    Top = 8
    Width = 27
    Height = 13
    Caption = 'pixels'
  end
  object gProgress: TGauge
    Left = 8
    Top = 589
    Width = 923
    Height = 18
    Anchors = [akLeft, akRight, akBottom]
    Progress = 0
  end
  object gProgressFrame: TGauge
    Left = 8
    Top = 613
    Width = 923
    Height = 18
    Anchors = [akLeft, akRight, akBottom]
    MaxValue = 200
    Progress = 0
  end
  object rgPalette: TRadioGroup
    Left = 8
    Top = 8
    Width = 320
    Height = 217
    Caption = ' Palette '
    Items.Strings = (
      'Windows 20 color'
      'Windows 256 color halftone'
      'Windows 4 grayscale'
      'Black/white monochrome'
      'Uniform 256 shade grayscale'
      'Netscape 216 color'
      'Optimal 2^n color'
      'Optimal 256 color windows')
    TabOrder = 0
    OnClick = ParamChanged
  end
  object rgDithering: TRadioGroup
    Left = 8
    Top = 231
    Width = 320
    Height = 194
    Caption = ' Dithering '
    Items.Strings = (
      'Nearest color matching w/o error correction'
      'Floyd Steinberg'
      'Stucki'
      'Sierra'
      'Jarvis, Judice & Ninke'
      'Stevenson & Arche'
      'Burkes')
    TabOrder = 1
    OnClick = ParamChanged
  end
  object grpTransparency: TGroupBox
    Left = 8
    Top = 432
    Width = 320
    Height = 65
    Caption = ' Transparency '
    TabOrder = 2
    object pbTransparentColor: TShape
      Left = 141
      Top = 37
      Width = 20
      Height = 20
      OnMouseUp = pbTransparentColorMouseUp
    end
    object rbOpacy: TRadioButton
      Left = 8
      Top = 17
      Width = 113
      Height = 17
      Caption = 'Opacy'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = ParamChanged
    end
    object rbTransparent: TRadioButton
      Left = 8
      Top = 40
      Width = 113
      Height = 17
      Caption = 'Transparent color:'
      TabOrder = 1
      OnClick = ParamChanged
    end
  end
  object seWidth: TSpinEdit
    Left = 362
    Top = 5
    Width = 65
    Height = 22
    MaxValue = 100000
    MinValue = 0
    TabOrder = 3
    Value = 0
    OnChange = ParamChanged
  end
  object seHeight: TSpinEdit
    Left = 449
    Top = 5
    Width = 65
    Height = 22
    MaxValue = 100000
    MinValue = 0
    TabOrder = 4
    Value = 0
    OnChange = ParamChanged
  end
  object btnSave: TButton
    Left = 856
    Top = 5
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100'...'
    Enabled = False
    TabOrder = 5
    OnClick = btnSaveClick
    ExplicitLeft = 716
  end
  object sb: TStatusBar
    Left = 0
    Top = 647
    Width = 939
    Height = 19
    Panels = <>
    ExplicitLeft = 480
    ExplicitTop = 344
    ExplicitWidth = 0
  end
  object ApplicationEvents1: TApplicationEvents
    OnIdle = ApplicationEvents1Idle
    Left = 280
    Top = 56
  end
  object SaveToGIFDialog: TSaveDialog
    DefaultExt = '.gif'
    Filter = 'GIF-'#1092#1072#1081#1083#1099' (*.gif)|*.gif|'#1042#1089#1077' '#1092#1072#1081#1083#1099'|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofNoChangeDir, ofPathMustExist, ofNoReadOnlyReturn, ofEnableSizing]
    Left = 872
    Top = 32
  end
  object dlgColor: TColorDialog
    Left = 136
    Top = 432
  end
end
