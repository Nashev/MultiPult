unit UtilsUnit;

interface

function FormatFileSize(ASize: Int64; AShowUnits: Boolean = True): string;
procedure InfoMsg(AText: string);
procedure TakeVersionInfo;

var
  VersionNameString, VersionCopyrightString: string;

implementation

uses Windows, SysUtils, Forms;

resourcestring
  rs_Byte1 = ' байт';
  rs_Byte2 = ' байта';
  rs_Byte5 = ' байт';

function FormatFileSize(ASize: Int64; AShowUnits: Boolean = True): string;
var
  i, j: Integer;
begin
  Result := IntToStr(ASize);

  // расставляем разделители тысячных разрядов
  // трюк: последние три считать по одному не надо (можно пропустить сразу),
  // и перед первым пробел не нужен
  j := 3;
  for i := Length(Result) - 2 downto 2 do
    if j < 3 then
      Inc(j)
    else
      begin
        Insert(FormatSettings.ThousandSeparator, Result, i);
        j := 1;
      end;
  if not AShowUnits then
    Exit;

  if (Length(Result) > 1) and (Result[Length(Result) - 1] = '1') then // *11..*19 байт
    Result := Result + rs_Byte5
  else
    case Ord(Result[Length(Result)]) - Ord('1') + 1 of
      1:     Result := Result + rs_Byte1;
      2..4:  Result := Result + rs_Byte2;
      0, 5..9: Result := Result + rs_Byte5;
    end;
end;


procedure InfoMsg(AText: string);
begin
  MessageBox(
    Application.MainForm.Handle,
    PChar(AText),
    PChar(Application.Title),
    MB_OK + MB_ICONINFORMATION + MB_SETFOREGROUND
  );
end;

procedure TakeVersionInfo;

  function AppFileName: string;
  var
    FileName: array [0 .. 255] of Char;
  begin
    if IsLibrary then
      begin
        GetModuleFileName(HInstance, FileName, SizeOf(FileName) - 1);
        Result := StrPas(FileName);
      end
    else
      Result := ParamStr(0);
  end;

type
  TLongVersion = record
    case Integer of
    0: (All: array[1..4 ] of Word);
    1: (MS, LS: LongInt);
  end;
var
  FileName: string;
  VersionInfoHandle: DWORD;
  VersionInfoSize: DWORD;
  VersionInfoBuffer: PByte;
  Len: UINT;
  FixedFileInfo: PVSFixedFileInfo;
  V: TLongVersion;
  Translation: Pointer;
  TranslationString: string;
  Copyright: Pointer;
begin
  VersionInfoBuffer := nil;
  // пробуем получить от файла:
  FileName := AppFileName;
  VersionInfoSize := GetFileVersionInfoSize(PWideChar(FileName), VersionInfoHandle);
  if VersionInfoSize > 0 then
    try
      GetMem(VersionInfoBuffer, VersionInfoSize);
      if not GetFileVersionInfo(PWideChar(FileName), VersionInfoHandle, VersionInfoSize, VersionInfoBuffer) then
        Exit;
      VerQueryValue(VersionInfoBuffer, '\', Pointer(FixedFileInfo), Len);
      V.MS := FixedFileInfo^.dwFileVersionMS;
      V.LS := FixedFileInfo^.dwFileVersionLS;
      with V do
        VersionNameString := Format('%d.%d.%d', [All[2], All[1], All[4]]);

      if VerQueryValue(VersionInfoBuffer, '\VarFileInfo\Translation', Translation, Len) then
        begin
          TranslationString := IntToHex(MakeLong(HiWord(LongInt(Translation^)), LoWord(LongInt(Translation^))), 8);
          if VerQueryValue(VersionInfoBuffer, PWideChar('\StringFileInfo\' + TranslationString + '\LegalCopyright'), Copyright, Len) then
            VersionCopyrightString := StrPas(PChar(Copyright));
        end;
    finally
      FreeMem(VersionInfoBuffer, VersionInfoSize);
    end;
end;

end.
