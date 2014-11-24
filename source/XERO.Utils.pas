unit XERO.Utils;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes;

function IsEmptyString(AValue: string): boolean;

function URLEncode(const AStr: String): String;
function URLDecode(const AStr: string): string;
function GetURLSeperator(AURL: string): string;

implementation

function GetURLSeperator(AURL: string): string;
begin
  Result := '?';
  if Pos('?', AURL) > 0 then
    Result := '&';
end;

function IsEmptyString(AValue: string): boolean;
begin
  Result := Trim(AValue) = '';
end;

function URLEncode(const AStr: String): String;
var
  i: integer;
begin
  Result := '';
  for i := 1 to length(AStr) do
    if not(AStr[i] in ['A' .. 'Z', 'a' .. 'z', '0', '1' .. '9', '-', '_', '~',
      '.']) then
      Result := Result + '%' + inttohex(ord(AStr[i]), 2)
    else
      Result := Result + AStr[i];
end;

// function URLDecode(AStr: string): string;
// var
// i, s, g: integer;
// begin
// Result := '';
//
// for i := 1 to length(AStr) do
// begin
//
// if AStr[i] = '%' then
// begin
// s := StrtoInt('$' + AStr[i + 1]) * 16;
// g := StrtoInt('$' + AStr[i + 2]);
//
// Result := Result + Chr(s + g);
// end
// else if not(((AStr[i - 1] = '%') and (AStr[i + 1] <> '%')) or
// ((AStr[i - 2] = '%') and (AStr[i - 1] <> '%') and (AStr[i + 1] = '%')) or
// ((AStr[i - 2] = '%') and (AStr[i - 1] <> '%') and (AStr[i + 1] <> '%')))
// then
// Result := Result + AStr[i];
//
// end;
// end;

function URLDecode(const AStr: string): string;
var
  i: integer;
  b: Byte;
begin
  Result := '';

  i := 1;
  while (i <= length(AStr)) do
  begin
    if (AStr[i] = '%') then
    begin
      try
        b := Byte(StrtoInt('$' + Copy(AStr, i + 1, 2)));
        Result := Result + ANSIChar(b);
        Inc(i, 2);
      except
        EXIT;
      end;
    end
    else
      Result := Result + AStr[i];

    Inc(i);
  end;
end;

end.
