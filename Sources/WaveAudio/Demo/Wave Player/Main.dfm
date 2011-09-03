object MainForm: TMainForm
  Left = 267
  Top = 240
  Width = 345
  Height = 180
  Caption = 'Wave Player'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object StartPosLabel: TLabel
    Left = 14
    Top = 83
    Width = 29
    Height = 13
    Caption = '0.00 s'
    ShowAccelChar = False
    Transparent = False
  end
  object AudioFormatLabel: TLabel
    Left = 14
    Top = 46
    Width = 3
    Height = 13
    ShowAccelChar = False
  end
  object EndPosLabel: TLabel
    Left = 294
    Top = 83
    Width = 29
    Height = 13
    Alignment = taRightJustify
    Caption = '0.00 s'
    ShowAccelChar = False
    Transparent = False
  end
  object btnPlay: TBitBtn
    Left = 14
    Top = 108
    Width = 100
    Height = 25
    Caption = 'Play'
    Enabled = False
    TabOrder = 2
    OnClick = btnPlayClick
  end
  object btnPause: TBitBtn
    Left = 118
    Top = 108
    Width = 100
    Height = 25
    Caption = 'Pause'
    Enabled = False
    TabOrder = 3
    OnClick = btnPauseClick
  end
  object btnStop: TBitBtn
    Left = 223
    Top = 108
    Width = 100
    Height = 25
    Caption = 'Stop'
    Enabled = False
    TabOrder = 4
    OnClick = btnStopClick
  end
  object Progress: TProgressBar
    Left = 14
    Top = 63
    Width = 309
    Height = 17
    TabOrder = 5
  end
  object btnSelectFile: TButton
    Left = 262
    Top = 22
    Width = 61
    Height = 21
    Caption = 'Browse...'
    TabOrder = 1
    OnClick = btnSelectFileClick
  end
  object WaveFileName: TEdit
    Left = 14
    Top = 22
    Width = 246
    Height = 21
    TabStop = False
    ReadOnly = True
    TabOrder = 0
  end
  object StockAudioPlayer: TStockAudioPlayer
    BufferLength = 100
    OnActivate = StockAudioPlayerActivate
    OnDeactivate = StockAudioPlayerDeactivate
    OnLevel = StockAudioPlayerLevel
    Left = 50
    Top = 5
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'wav'
    Filter = 'Wave Files (*.wav)|*.wav|All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select Wave File'
    Left = 19
    Top = 5
  end
end
