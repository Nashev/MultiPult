unit UtilsUnit;

interface

function FormatFileSize(ASize: Int64; AShowUnits: Boolean = True): string;

implementation

uses SysUtils;

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

end.
