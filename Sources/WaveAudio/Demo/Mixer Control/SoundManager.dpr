program SoundManager;

uses
  Forms,
  guiSoundManagerMain in 'guiSoundManagerMain.pas' {frmSoundManagerMain},
  guiAudioDevice in 'guiAudioDevice.pas' {frmAudioDevice},
  guiMixerLine in 'guiMixerLine.pas' {frmMixerLine};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmSoundManagerMain, frmSoundManagerMain);
  Application.Run;
end.
