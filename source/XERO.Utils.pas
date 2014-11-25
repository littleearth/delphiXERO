unit XERO.Utils;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes;

function IsEmptyString(AValue: string): boolean;

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
