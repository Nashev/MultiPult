object MP3ConvertForm: TMP3ConvertForm
  Left = 192
  Top = 114
  BorderWidth = 8
  Caption = 'mp3 -> wav converter'
  ClientHeight = 93
  ClientWidth = 375
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblStstus: TLabel
    Left = 0
    Top = 0
    Width = 375
    Height = 13
    Align = alTop
    Caption = 'lblStstus'
    ExplicitWidth = 39
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 76
    Width = 375
    Height = 17
    Align = alBottom
    TabOrder = 0
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
