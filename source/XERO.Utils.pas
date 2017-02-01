unit XERO.Utils;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes;

function IsEmptyString(AValue: string): boolean;
function StripCRLLF(AValue: string): string;

function URLEncode(const AStr: String): String;
function URLDecode(const AStr: string): string;
function GetURLSeperator(AURL: string): string;
function EncodeHTML(AValue: String): string;
function DecodeHTML(AValue: String): string;

implementation

uses
  StrUtils, System.IOUtils,
  Winapi.ShellAPI,
  IdURI;

function StripCRLLF(AValue: string): string;
begin
  Result := AValue;
  Result := StringReplace(Result, #10, '', [rfReplaceAll]);
  Result := StringReplace(Result, #13, '', [rfReplaceAll]);
end;

function GetURLSeperator(AURL: string): string;
begin
  Result := '?';
  if Pos('?', AURL) > 0 then
    Result := '&';
end;

function IsEmptyString(AValue: string): boolean;
var
  ch : Char;
begin
  result := true;
  for ch in Avalue do
  begin
    case ch of
    ' ': ;
    else
      result := false;
      break;
    end;
  end;
end;

function URLEncode(const AStr: String): String;
var
  i: integer;
begin
  Result := '';
  for i := 1 to Length(AStr) do
    case AStr[i] of
    'A' .. 'Z', 'a' .. 'z', '0', '1' .. '9', '-', '_', '~', '.':
      Result := Result + AStr[i];
    else
      Result := Result + '%' + inttohex(ord(AStr[i]), 2);
    end;
end;

function URLDecode(const AStr: string): string;
var
  i: integer;
  b: Byte;
begin
  Result := '';

  i := 1;
  while (i <= Length(AStr)) do
  begin
    if (AStr[i] = '%') then
    begin
      try
        b := Byte(StrtoInt('$' + Copy(AStr, i + 1, 2)));
        Result := Result + Char(ANSIChar(b));
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

function EncodeHTML(AValue: String): string;
begin
  Result := ReplaceStr(AValue, '&', '&amp;');
  Result := ReplaceStr(Result, '<', '&lt;');
  Result := ReplaceStr(Result, '>', '&gt;');
  Result := ReplaceStr(Result, chr(39), '&apos;');
  Result := ReplaceStr(Result, '"', '&quot;');
  // Result := ReplaceStr(Result, #13, '&#13;'); // CR
  // Result := ReplaceStr(Result, #10, '&#10;'); // LF
  Result := ReplaceStr(Result, #13, '&#xD;'); // CR
  Result := ReplaceStr(Result, #10, '&#xA;'); // LF
end;

function DecodeHTML(AValue: String): string;
begin
  Result := ReplaceStr(AValue, '&lt;', '<');
  Result := ReplaceStr(Result, '&gt;', '>');
  Result := ReplaceStr(Result, '&apos;', chr(39));
  Result := ReplaceStr(Result, '&quot;', '"');
  Result := ReplaceStr(Result, '&amp;', '&');
  Result := ReplaceStr(Result, '&#10;', #10);
  Result := ReplaceStr(Result, '&#13;', #13);
  Result := ReplaceStr(Result, '&#xA;', #10);
  Result := ReplaceStr(Result, '&#xD;', #13);
end;

end.
