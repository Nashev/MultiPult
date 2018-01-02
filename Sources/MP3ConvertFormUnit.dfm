inherited MP3ConvertForm: TMP3ConvertForm
  Caption = 'mp3 -> wav converter'
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  Visible = False
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
