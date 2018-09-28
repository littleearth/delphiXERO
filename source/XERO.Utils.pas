unit XERO.Utils;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes;

function IsEmptyString(AValue: string): boolean;
function StripCRLLF(AValue: string): string;
function StripNonNumeric(const AValue: string; AAllowDecimal: boolean = False;
  AAllowNegative: boolean = False): string;

function URLEncode(const AStr: String): String;
function URLDecode(const AStr: string): string;
function GetURLSeperator(AURL: string): string;
function EncodeHTML(AValue: String): string;
function DecodeHTML(AValue: String): string;

function GetUserAppDataDir: string;
function GetShellFolderPath(AFolder: integer): string;
function ExecuteFile(const Operation, FileName, Params, DefaultDir: string;
  ShowCmd: word): integer;
function CheckDirectoryExists(ADirectory: string; ACreate: boolean): boolean;

implementation

uses
  StrUtils, System.IOUtils,
  Winapi.ShellAPI, Vcl.Forms,
  IdURI, Winapi.ShlObj;

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
  ch: Char;
begin
  Result := true;
  for ch in AValue do
  begin
    case ch of
      ' ':
        ;
    else
      Result := False;
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

function StripNonNumeric(const AValue: string; AAllowDecimal: boolean = False;
  AAllowNegative: boolean = False): string;
var
  i: integer;
begin
  Result := '';
  if Trim(AValue) <> '' then
  begin

    for i := 1 to Length(AValue) do
    begin
      if (CharInSet(AValue[i], ['0' .. '9'])) or
        ((AAllowDecimal) and (AValue[i] = '.')) or
        ((AAllowNegative) and (AValue[i] = '-')) then
      begin
        Result := Result + AValue[i];
      end;
    end;
  end;
  Result := Trim(Result);
end;

function GetUserAppDataDir: string;
begin
  Result := IncludeTrailingPathDelimiter
    (IncludeTrailingPathDelimiter(GetShellFolderPath(CSIDL_APPDATA)) +
    Application.Title);
  CheckDirectoryExists(Result, true);
end;

function GetShellFolderPath(AFolder: integer): string;
const
  SHGFP_TYPE_CURRENT = 0;
var
  path: array [0 .. MAX_PATH] of Char;
begin
  if SUCCEEDED(SHGetFolderPath(0, AFolder, 0, SHGFP_TYPE_CURRENT, @path[0]))
  then
    Result := path
  else
    Result := '';
end;

function CheckDirectoryExists(ADirectory: string; ACreate: boolean): boolean;
begin
  try
    if ACreate then
    begin
      if not DirectoryExists(ADirectory) then
      begin
        ForceDirectories(ADirectory);
      end;
    end;
  finally
    Result := DirectoryExists(ADirectory);
  end;
end;

function ExecuteFile(const Operation, FileName, Params, DefaultDir: string;
  ShowCmd: word): integer;
var
  zFileName, zParams, zDir: array [0 .. 255] of Char;
begin
  Result := ShellExecute(Application.Handle, PChar(Operation),
    StrPCopy(zFileName, FileName), StrPCopy(zParams, Params),
    StrPCopy(zDir, DefaultDir), ShowCmd);
end;

end.
