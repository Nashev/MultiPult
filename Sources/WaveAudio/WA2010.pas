{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit WA2010;

interface

uses
  WaveACM, WaveIn, WaveOut, WavePlayers, WaveRecorders, WaveRedirector, 
  WaveStorage, WaveTimer, WaveUtils, WaveIO, WaveMixer, WaveReg, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('WaveReg', @WaveReg.Register);
end;

initialization
  RegisterPackage('WA2010', @Register);
end.
