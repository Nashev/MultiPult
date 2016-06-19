object MP3ConvertForm: TMP3ConvertForm
  Left = 192
  Top = 114
  BorderWidth = 8
  Caption = 'mp3 -> wav converter'
  ClientHeight = 95
  ClientWidth = 616
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  DesignSize = (
    616
    95)
  PixelsPerInch = 96
  TextHeight = 13
  object lblStstus: TLabel
    Left = 0
    Top = 0
    Width = 616
    Height = 13
    Align = alTop
    Caption = #1050#1086#1085#1074#1077#1088#1090#1072#1094#1080#1103' '#1074#1099#1073#1088#1072#1085#1085#1086#1075#1086' MP3 '#1074' WAV '#1074' '#1087#1072#1087#1082#1091' '#1089' '#1092#1086#1090#1086#1075#1088#1072#1092#1080#1103#1084#1080':'
    ExplicitWidth = 324
  end
  object lblMP3: TLabel
    Left = 0
    Top = 13
    Width = 616
    Height = 13
    Align = alTop
    Caption = 'lblMP3'
    ExplicitWidth = 32
  end
  object lblWav: TLabel
    Left = 0
    Top = 26
    Width = 616
    Height = 13
    Align = alTop
    Caption = 'lblWav'
    ExplicitLeft = 188
    ExplicitTop = 55
    ExplicitWidth = 33
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 39
    Width = 616
    Height = 17
    Align = alTop
    TabOrder = 0
    ExplicitTop = 76
    ExplicitWidth = 375
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
    Visible = False
    OnClick = btnCancelClick
  end
  object MP3In1: TMP3In
    Loop = False
    EndSample = -1
    StartSample = 0
    HighPrecision = False
    OutputChannels = cnMonoOrStereo
    Left = 160
    Top = 8
  end
  object WaveOut1: TWaveOut
    Input = MP3In1
    OnDone = WaveOut1Done
    OnProgress = WaveOut1Progress
    OnThreadException = WaveOut1ThreadException
    ShareMode = 0
    WavType = wtPCM
    BlockSize = 512
    CreateNonMsHeaders = True
    FileMode = foRewrite
    Left = 192
    Top = 8
  end
end
