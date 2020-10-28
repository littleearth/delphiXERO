unit XERO.PKCE;

{$INCLUDE 'XERO.inc'}

interface

uses
  XERO.Types, Sysutils;

type
  TXEROPKCE = class(TXEROComponent)
  public
    class function GenerateCodeVerifier: string;
    class function GetCodeChallenge(ACodeVerifier: string): string;
  end;

implementation

uses
  XERO.Utils, System.Hash, System.NetEncoding;

{ TXEROPKCE }

class function TXEROPKCE.GenerateCodeVerifier: string;
var
  Lidx: integer;
  LLength: integer;
  LAllowCharacters: string;
begin
  Result := '';
  LAllowCharacters :=
    '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-._~';
  LLength := 43 + Random(85);
  for Lidx := 0 to LLength do
  begin
    Result := Result + LAllowCharacters[Random(Length(LAllowCharacters)) + 1];
  end;
end;

class function TXEROPKCE.GetCodeChallenge(ACodeVerifier: string): string;
var
  LSHA256: string;
  LSHA256Bytes: TBytes;

begin
  Result := '';
  if not IsEmptyString(ACodeVerifier) then
  begin
    LSHA256Bytes := THashSHA2.GetHashBytes(ACodeVerifier);
    LSHA256 := TNetEncoding.Base64.EncodeBytesToString(LSHA256Bytes);
    Result := StringReplace(LSHA256, '=', '', [rfReplaceAll]);
    Result := StringReplace(Result, '+', '-', [rfReplaceAll]);
    Result := StringReplace(Result, '/', '_', [rfReplaceAll]);
  end;
end;

end.
