unit XERO.RSAUtils;

interface

uses
  Classes,
  XERO.CipherRSA,
  XERO.HugeInt;

type
  IASN1Reader = interface
    ['{B5F47A2B-A608-4C7B-8D18-8EF8B0FB0737}']
    function PeekLength: Int64;
    function PeekID: Byte;
    function ReadInteger: HugeInt;
    procedure ReadNull;
    function ReadObjectID: String;
    function ReadOctetString: String;
    function ReadSequence: IASN1Reader;
    function ReadString: String;
    procedure ReadTag(var aID: Byte; var aLength: Int64);
  end;

  TASN1Reader = class(TInterfacedObject, IASN1Reader)
  private
    fMaxPos: Int64;
    fOwnStream: Boolean;
    fStream: TStream;
    procedure CheckEOF;
    procedure ExpectID(const aID: Byte); overload;
    procedure ExpectID(const aID: Byte; var aLength: Int64); overload;
    function ReadLength: Int64;
  public
    constructor Create(const aHexStr: String); overload;
    constructor Create(const aStream: TStream;
      const aMaxPos: Int64 = -1); overload;
    destructor Destroy; override;
    procedure GetManifest(const aStrings: TStrings);
    function PeekLength: Int64;
    function PeekID: Byte;
    function ReadInteger: HugeInt;
    procedure ReadNull;
    function ReadObjectID: String;
    function ReadOctetString: String;
    function ReadSequence: IASN1Reader;
    function ReadString: String;
    procedure ReadTag(var aID: Byte; var aLength: Int64);
  end;

procedure RSALoadPEMData(const aInput: TStream;
  const aOutput: TStream); overload;
procedure RSALoadPEMData(const aFilename: String;
  const aStream: TStream); overload;
procedure RSAPublicKeyFromPrivate(const aKey: TRSAPrivateKey;
  var aPublicKey: TRSAPublicKey);
procedure RSAReadASN1PrivateKey(const aBase64: ANSIString;
  var aKey: TRSAPrivateKey); overload;
procedure RSAReadASN1PrivateKey(const aStream: TStream;
  var aKey: TRSAPrivateKey; const aFromBeginning: Boolean = TRUE); overload;
function RSAPKCS1v15AsBase64(const aBuf: Pointer; const aBufSize: Integer;
  const aKey: TRSAPublicKey): ANSIString; overload;
function RSAPKCS1v15AsBase64(const aString: String; const aKey: TRSAPublicKey)
  : ANSIString; overload;
function RSAPKCS1v15AsHex(const aBuf: Pointer; const aBufSize: Integer;
  const aKey: TRSAPublicKey): ANSIString; overload;
function RSAPKCS1v15AsHex(const aString: String; const aKey: TRSAPublicKey)
  : ANSIString; overload;
function RSAPKCS1v15FromHexAsHex(const aHexString: String;
  const aKey: TRSAPublicKey): ANSIString;

implementation

uses
  Math,
  SysUtils,
  Windows,
  XERO.Base64,
  XERO.SHA1;

const
  ID_INTEGER = $02;
  ID_OCTET_STRING = $04;
  ID_NULL = $05;
  ID_OBJECT_IDENTIFIER = $06;
  ID_SEQUENCE = $30;

const
  IDNAME: array [$02 .. $30] of String = (
    { 02 } 'INTEGER',
    { 03 } '',
    { 04 } 'OCTET STRING',
    { 05 } 'NULL',
    { 06 } 'OBJECT IDENTIFIER',
    { 07 } '',
    { 08 } '',
    { 09 } '',
    { 0a } '',
    { 0b } '',
    { 0c } '',
    { 0d } '',
    { 0e } '',
    { 0f } '',
    { 10 } '',
    { 11 } '',
    { 12 } '',
    { 13 } '',
    { 14 } '',
    { 15 } '',
    { 16 } '',
    { 17 } '',
    { 18 } '',
    { 19 } '',
    { 1a } '',
    { 1b } '',
    { 1c } '',
    { 1d } '',
    { 1e } '',
    { 1f } '',
    { 20 } '',
    { 21 } '',
    { 22 } '',
    { 23 } '',
    { 24 } '',
    { 25 } '',
    { 26 } '',
    { 27 } '',
    { 28 } '',
    { 29 } '',
    { 2a } '',
    { 2b } '',
    { 2c } '',
    { 2d } '',
    { 2e } '',
    { 2f } '',
    { 30 } 'SEQUENCE');

  { TASN1Reader ------------------------------------------------------------------------------------ }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
constructor TASN1Reader.Create(const aHexStr: String);
begin
  inherited Create;

  fMaxPos := -1;
  fOwnStream := TRUE;

  fStream := TMemoryStream.Create;
  fStream.Size := Length(aHexStr) div 2;

  HexToBin(PChar(aHexStr), TMemoryStream(fStream).Memory, fStream.Size);
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
constructor TASN1Reader.Create(const aStream: TStream; const aMaxPos: Int64);
begin
  inherited Create;

  fMaxPos := aMaxPos;
  fStream := aStream;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
destructor TASN1Reader.Destroy;
begin
  if fOwnStream then
    FreeAndNIL(fStream);

  inherited;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
procedure TASN1Reader.CheckEOF;
begin
  if (fStream.Position = fStream.Size) or
    ((fMaxPos <> -1) and (fStream.Position > fMaxPos)) then
    raise Exception.Create('Unexpected end of file.  No more data');
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
procedure TASN1Reader.ExpectID(const aID: Byte);
var
  b: Byte;
begin
  CheckEOF;

  fStream.Read(b, 1);
  if (b <> aID) then
    raise Exception.CreateFmt
      ('Expected %s identifier ($%.2x) in stream.  Found $%.2x',
      [IDNAME[aID], aID, b]);
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
procedure TASN1Reader.ExpectID(const aID: Byte; var aLength: Int64);
begin
  ExpectID(aID);
  aLength := ReadLength;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
procedure TASN1Reader.GetManifest(const aStrings: TStrings);
begin

end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
function TASN1Reader.PeekID: Byte;
begin
  CheckEOF;

  fStream.Read(result, 1);
  fStream.Seek(-1, soFromCurrent);
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
function TASN1Reader.PeekLength: Int64;
var
  p: Int64;
begin
  CheckEOF;

  result := 0;

  p := fStream.Position;
  try
    result := ReadLength;

  finally
    fStream.Position := p;
  end;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
function TASN1Reader.ReadInteger: HugeInt;
var
  i: Integer;
  len: Int64;
  data: array of Byte;
  dataP: array of Byte;
begin
  ExpectID(ID_INTEGER, len);

  HugeWordInitZero(result.Value);

  if (len = 0) then
    EXIT;

  if ((len mod 4) = 0) then
    HugeWordSetSize(result.Value, len div 4)
  else
    HugeWordSetSize(result.Value, (len div 4) + 1);

  SetLength(data, HugeWordGetBitCount(result.Value) div 8);
  data[0] := 0;
  data[1] := 0;
  data[2] := 0;
  data[3] := 0;
  fStream.Read(data[(4 - (len mod 4)) mod 4], len);

  SetLength(dataP, Length(data));
  for i := 0 to Pred(Length(data)) do
    dataP[i] := data[Length(data) - (i + 1)];

  CopyMemory(HugeWordGetFirstElementPtr(result.Value), @dataP[0],
    Length(dataP));
  HugeWordNormalise(result.Value);
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
function TASN1Reader.ReadLength: Int64;
var
  i: Integer;
  b: Byte;
  bs: Integer;
  p: Int64;
begin
  CheckEOF;

  fStream.Read(b, 1);

  // High bit NOT set >> The length byte value is the length

  if (b and $80) = 0 then
  begin
    result := b;
    EXIT;
  end;

  // High bit SET >> Bits 7-1 indicate the number of additional length bytes

  // Each byte in the length data then encodes the actual length
  // as a base256 integer

  result := 0;
  bs := (b and $7F);

  for i := Pred(bs) downto 0 do
  begin
    fStream.Read(b, 1);

    p := Trunc(Power(256, i));
    result := result + (b * p);
  end;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
procedure TASN1Reader.ReadNull;
var
  len: Int64;
begin
  ExpectID(ID_NULL, len);
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
function TASN1Reader.ReadObjectID: String;
var
  len: Int64;
  b: array of Byte;
begin
  ExpectID(ID_OBJECT_IDENTIFIER, len);

  SetLength(b, len);
  fStream.Read(b[0], len);

  result := '';
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
function TASN1Reader.ReadOctetString: String;
var
  len: Int64;
  b: array of Byte;
begin
  ExpectID(ID_OCTET_STRING, len);

  SetLength(b, len);
  fStream.Read(b[0], len);

  SetLength(result, len * 2);
  BinToHex(Pointer(@b[0]), PChar(result), len);
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
function TASN1Reader.ReadSequence: IASN1Reader;
var
  len: Int64;
begin
  ExpectID(ID_SEQUENCE, len);

  result := TASN1Reader.Create(fStream, fStream.Position + len - 1);
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
function TASN1Reader.ReadString: String;
begin
  CheckEOF;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
procedure TASN1Reader.ReadTag(var aID: Byte; var aLength: Int64);
begin
  CheckEOF;

  fStream.Read(aID, 1);

  aLength := ReadLength;
end;

{ ------------------------------------------------------------------------------------------------ }

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
procedure RSALoadPEMData(const aInput: TStream; const aOutput: TStream);
var
  s: ANSIString;
  contentType: ANSIString;
  content: ANSIString;

  procedure Expect(const aString: ANSIString); overload;
  begin
    if (Copy(s, 1, Length(aString)) <> aString) then
      raise Exception.Create('Expected ''' + aString + ''' in file.  Found ''' +
        Copy(s, 1, Length(aString)) + '''');

    Delete(s, 1, Length(aString));
  end;

  procedure Expect(var aSubString: ANSIString;
    const aString: ANSIString); overload;
  var
    p: Integer;
  begin
    p := Pos(aString, s);

    if (p = 0) then
      raise Exception.Create('Expected ''' + aString +
        ''' in file but was not found');

    aSubString := Copy(s, 1, p - 1);
    Delete(s, 1, p + Length(aString) - 1);
  end;

var
  buf: Pointer;
  bufSize: Integer;
begin
  SetLength(s, aInput.Size - aInput.Position);
  aInput.Read(s[1], Length(s));

  s := StringReplace(s, #13, '', [rfReplaceAll]);
  s := StringReplace(s, #10, '', [rfReplaceAll]);

  Expect('-----');
  Expect('BEGIN ');
  Expect(contentType, '-----');
  Expect(content, '-----END ' + contentType + '-----');

  bufSize := (Length(content) * 3) div 4;
  buf := AllocMem(bufSize);
  try
    bufSize := Base64Decode(@content[1], buf, Length(content));
    aOutput.Write(buf^, bufSize);

  finally
    FreeMem(buf);
  end;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
procedure RSALoadPEMData(const aFilename: String; const aStream: TStream);
var
  strm: TFileStream;
begin
  strm := TFileStream.Create(aFilename, fmOpenRead);
  try
    RSALoadPEMData(strm, aStream);

  finally
    strm.Free;
  end;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
procedure RSAPublicKeyFromPrivate(const aKey: TRSAPrivateKey;
  var aPublicKey: TRSAPublicKey);
begin
  RSAPublicKeyInit(aPublicKey);
  try
    HugeWordAssign(aPublicKey.Modulus, aKey.Modulus);
    HugeWordAssign(aPublicKey.Exponent, aKey.Exponent);
    HugeWordNormalise(aPublicKey.Modulus);
    HugeWordNormalise(aPublicKey.Exponent);

    aPublicKey.KeySize := aKey.KeySize;

  except
    RSAPublicKeyFinalise(aPublicKey);
    raise;
  end;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
procedure RSAReadASN1PrivateKey(const aBase64: ANSIString;
  var aKey: TRSAPrivateKey);
var
  strm: TMemoryStream;
begin
  strm := TMemoryStream.Create;
  try
    strm.Size := (Length(aBase64) * 3) div 4;

    Base64Decode(Pointer(aBase64), strm.Memory, Length(aBase64));

    RSAReadASN1PrivateKey(strm, aKey, TRUE);

  finally
    strm.Free;
  end;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
procedure RSAReadASN1PrivateKey(const aStream: TStream;
  var aKey: TRSAPrivateKey; const aFromBeginning: Boolean);
{
  Reads a PRIVATE KEY record from an ASN.1 formatted stream according to the
  ASN.1 type specification for RSAPrivateKey ::=

  SEQUENCE
  [
  version           <version>
  modulus           INTEGER
  publicExponent    INTEGER
  privateExponent   INTEGER
  prime1            INTEGER
  prime2            INTEGER
  exponent1         INTEGER
  exponent2         INTEGER
  coefficient       INTEGER
  ]

  <version> ::= INTEGER
}
var
  asn: IASN1Reader;
  s: String;
begin
  if aFromBeginning then
    aStream.Position := 0;

  RSAPrivateKeyInit(aKey);
  try
    asn := TASN1Reader.Create(aStream);
    asn := asn.ReadSequence;

    asn.ReadInteger; // VERSION - currently discarded

    if asn.PeekID = ID_SEQUENCE then
    begin
      with asn.ReadSequence do
      begin
        ReadObjectID;
        ReadNull;
      end;

      s := asn.ReadOctetString;
      asn := TASN1Reader.Create(s);
      asn.ReadSequence;
      asn.ReadInteger; // VERSION - currently discarded
    end;

    HugeWordAssign(aKey.Modulus, asn.ReadInteger.Value);
    HugeWordAssign(aKey.PublicExponent, asn.ReadInteger.Value);
    HugeWordAssign(aKey.Exponent, asn.ReadInteger.Value);
    HugeWordAssign(aKey.Prime1, asn.ReadInteger.Value);
    HugeWordAssign(aKey.Prime2, asn.ReadInteger.Value);
    HugeWordAssign(aKey.Exponent1, asn.ReadInteger.Value);
    HugeWordAssign(aKey.Exponent2, asn.ReadInteger.Value);
    HugeWordAssign(aKey.Coefficient, asn.ReadInteger.Value);

    aKey.KeySize := HugeWordGetBitCount(aKey.Modulus);

  except
    RSAPrivateKeyFinalise(aKey);
    raise;
  end;
end;

function RSAPKCS1v15AsBase64(const aBuf: Pointer; const aBufSize: Integer;
  const aKey: TRSAPublicKey): ANSIString;
var
  i: Integer;
  h: ANSIString;
  buf: array of Byte;
begin
  h := RSAPKCS1v15AsHex(aBuf, aBufSize, aKey);
  SetLength(buf, Length(h) div 2);
  HexToBin(PANSIChar(@h[1]), @buf[0], Length(h));

  SetLength(result, Length(buf) * 2);
  i := Base64Encode(@buf[0], PANSIChar(@result[1]), Length(buf));
  SetLength(result, i);
end;

function RSAPKCS1v15AsBase64(const aString: String; const aKey: TRSAPublicKey)
  : ANSIString;
var
  i: Integer;
  h: ANSIString;
  buf: array of Byte;
begin
  h := RSAPKCS1v15AsHex(aString, aKey);
  SetLength(buf, Length(h) div 2);
  HexToBin(PANSIChar(@h[1]), @buf[0], Length(h));

  SetLength(result, Length(buf) * 2);
  i := Base64Encode(@buf[0], PANSIChar(@result[1]), Length(buf));
  SetLength(result, i);
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
function RSAPKCS1v15AsHex(const aBuf: Pointer; const aBufSize: Integer;
  const aKey: TRSAPublicKey): ANSIString;
var
  msg: TRSAMessage;
  sig: TRSAMessage;
  sha1: TDCP_sha1;
  s: ANSIString;
  pad: ANSIString;
  digest: array of Byte;
  hex: ANSIString;
begin
  HugeWordInit(msg);
  HugeWordInit(sig);
  try
    sha1 := TDCP_sha1.Create(NIL);
    try
      sha1.Init;
      sha1.Update(aBuf^, aBufSize);

      SetLength(digest, sha1.GetHashSize div 8);
      sha1.Final(digest[0]);

      SetLength(hex, Length(digest) * 2);
      BinToHex(PANSIChar(@digest[0]), PANSIChar(@hex[1]), Length(digest));

    finally
      sha1.Free;
    end;

    s := '00' + '3021300906052b0e03021a05000414' + UTF8Encode(Uppercase(hex));

    SetLength(pad, (aKey.KeySize div 4) - Length(s) - 4);
    FillMemory(@pad[1], Length(pad), Byte('F'));

    s := '0001' + pad + s;

    HexToHugeWord(s, msg);
    HugeWordNormalise(msg);

    RSAEncryptMessage(aKey, msg, sig);
    HugeWordNormalise(sig);

    result := HugeWordToHex(sig);

  finally
    HugeWordFinalise(msg);
    HugeWordFinalise(sig);
  end;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
function RSAPKCS1v15AsHex(const aString: String; const aKey: TRSAPublicKey)
  : ANSIString;
var
  s: ANSIString;
begin
  s := UTF8Encode(aString);
  result := RSAPKCS1v15AsHex(Pointer(@s[1]), Length(s), aKey);
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
function RSAPKCS1v15FromHexAsHex(const aHexString: String;
  const aKey: TRSAPublicKey): ANSIString;
var
  buf: Pointer;
  bufSize: Integer;
begin
  bufSize := Length(aHexString) div 2;
  buf := AllocMem(bufSize);
  try
    HexToBin(PChar(aHexString), buf, bufSize);
    result := RSAPKCS1v15AsHex(buf, bufSize, aKey);

  finally
    FreeMem(buf);
  end;
end;

end.
