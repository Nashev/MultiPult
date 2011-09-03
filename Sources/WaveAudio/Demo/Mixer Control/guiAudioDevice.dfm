object frmAudioDevice: TfrmAudioDevice
  Left = 259
  Top = 144
  Align = alLeft
  AutoSize = True
  BorderStyle = bsNone
  Caption = 'frmAudioDevice'
  ClientHeight = 209
  ClientWidth = 115
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object AudioMixer: TAudioMixer
    OnControlChange = AudioMixerControlChange
    Left = 43
    Top = 24
  end
end
