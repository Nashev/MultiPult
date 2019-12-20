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
  object lblDittering: TLabel
    Left = 8
    Top = 38
    Width = 48
    Height = 13
    Caption = 'Dittering: '
    FocusControl = edDithering
  end
  object lblPalette: TLabel
    Left = 8
    Top = 8
    Width = 41
    Height = 13
    Caption = 'Palette: '
    FocusControl = edPalette
  end
  object edPalette: TComboBox
    Left = 59
    Top = 8
    Width = 272
    Height = 21
    ItemIndex = 0
    TabOrder = 0
    Text = 'Windows 20 color'
    OnClick = ParamChanged
    Items.Strings = (
      'Windows 20 color'
      'Windows 256 color halftone'
      'Windows 4 grayscale'
      'Black/white monochrome'
      'Uniform 256 shade grayscale'
      'Netscape 216 color'
      'Optimal 2^n color'
      'Optimal 256 color windows')
  end
  object edDithering: TComboBox
    Left = 59
    Top = 35
    Width = 272
    Height = 21
    ItemIndex = 0
    TabOrder = 1
    Text = 'Nearest color matching w/o error correction'
    OnClick = ParamChanged
    Items.Strings = (
      'Nearest color matching w/o error correction'
      'Floyd Steinberg'
      'Stucki'
      'Sierra'
      'Jarvis, Judice & Ninke'
      'Stevenson & Arche'
      'Burkes')
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
  end
  object sb: TStatusBar
    Left = 0
    Top = 647
    Width = 939
    Height = 19
    Panels = <>
  end
  object ApplicationEvents1: TApplicationEvents
    OnIdle = ApplicationEvents1Idle
    Left = 792
    Top = 80
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
