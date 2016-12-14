{: Miscellaneous utilties for parsing
}
unit XERO.MiscUtil;

interface

uses
  // System
  Classes, SysUtils;

type
  // Date/Time encoding/decoding parts.
  TXMLDatePart = (xdpDate, xdpTime);
  TXMLDateParts = set of TXMLDatePart;

  // Helper for decoding a parameter string as a
  // list of values.
  THTMLParameterStrings = class(TStringList)
  public
    // Assign and parse a HTML parameter string.
    procedure DecodeAndSet( const AValue : String);
  end;

// Output date/time in standard XML format
function XMLDateTimeToStr( ADate : TDateTime; Parts : TXMLDateParts = [xdpDate, xdpTime]) : String;
// Parse strict XML date/time string to TDateTime
function XMLStrToDateTime( const StrVal : String; Parts : TXMLDateParts = [xdpDate, xdpTime]): TDateTime;

// Set a HTML parameter into a list for output to a HTML call.
procedure SetHTMLParam(parameters : TStrings;const param, Data : String);

// Add a HTML parameter to a list of parameters
procedure AddHTMLParam(var parameters : String;const param, Data : String);

// Optionally Include a trailing URI separator ('/')
function IncludeTrailingURIDelimeter( const uri : String ) : String;

// Convert JSON Dates including Microsoft Date() format
function ConvertJSONDate(const ADate : String) : TDateTime;

function DateTimeAsMicrosoftJSONDate(ADateTime : TDateTime) : string;

implementation

uses
  // System
  StrUtils,
  DateUtils,

  // Indy
  idURI
  ;

function XMLDateTimeToStr( ADate : TDateTime; Parts : TXMLDateParts = [xdpDate, xdpTime]) : String;
  const
    CDateTimeFmt = 'yyyy-mm-dd"T"hh:nn:ss';
    CDateFmt = 'yyyy-mm-dd';
    CTimeFmt = 'hh:nn:ss';
begin
  if Parts = [xdpDate, xdpTime] then
    result := FormatDateTime(CDateTimeFmt, ADate)
  else if Parts = [xdpDate] then
    result := FormatDateTime(CDateFmt, ADate)
  else if Parts = [xdpTime] then
    result := FormatDateTime(CTimeFmt, ADate)
  else
    result := '';
end;

function XMLStrToDateTime( const StrVal : String; Parts : TXMLDateParts = [xdpDate, xdpTime]): TDateTime;
  procedure RaiseUnexpected(CH: Char);
  begin
    Raise Exception.CreateFmt('Unexpected character %s', [CH]);
  end;
type
  TParseElement = (peInit, peYear, peYearSep, peMonth, peMonthSep, peDay,
    peDaySep, peTimeInit,
    peHour, peHourSep, peMinute, peMinuteSep, peSecond, peFinish);
var
  State : TParseElement;
  ch : char;
  digit,
  dd, mm, yyyy, hh, nn, ss : Word;
  curVal : Integer;
  procedure StoreValue;
  begin
    case state of
      peYear:
        if curVal > 3000 then
          raise Exception.CreateFmt('Invalid Year: %d', [curVal])
        else
          yyyy := curVal;
      peMonth:
        if (curVal = 0) or (curVal > 12) then
          raise Exception.CreateFmt('Invalid Month: %d', [curVal])
        else
          mm := curVal;
      peDay:
        if (curVal = 0) or (curVal > 31) then
          raise Exception.CreateFmt('Invalid Day: %d', [curVal])
        else
          dd := curVal;
      peHour:
        if (curVal > 23) then
          raise Exception.CreateFmt('Invalid Hour: %d', [curVal])
        else
          hh := curVal;
      peMinute:
        if (curVal > 59) then
          raise Exception.CreateFmt('Invalid Minute: %d', [curVal])
        else
          nn := curVal;
      peSecond:
        if (curVal > 59) then
          raise Exception.CreateFmt('Invalid Second: %d', [curVal])
        else
          ss := curVal;
    end;
    curVal := 0;
  end;
begin
  if xdpDate in Parts then
    State := peInit
  else
    State := peTimeInit;
  dd := 0;
  mm := 0;
  yyyy := 0;
  hh  := 0;
  nn  := 0;
  ss := 0;

  curVal := 0;
  for ch in StrVal do
  begin
    case ch of
      '0'..'9':
        begin
          case state of
            peInit:    state := peYear;
            peYearSep: state := peMonth;
            peMonthSep:state := peDay;

            peTimeInit,
            peDaySep:  state := peHour;
            peHourSep: state := peMinute;
            peMinuteSep: state := peSecond;

            peFinish:
              break; // ?? error
          end;
          digit := ord(ch)-ord('0');
          if curVal = 0 then
            curVal := digit
          else
            curVal := (curVal * 10) + digit;
        end;
      '-', '.':
        case state of
        peYear, peMonth:
          begin
            StoreValue;
            State := Succ(State);
          end;
        else
          RaiseUnexpected(ch);
        end;
      ':':
        case state of
          peHour, peMinute:
            begin
              StoreValue;
              State := Succ(State);
            end;
        else
          RaiseUnexpected(ch);
        end;

      'T':
        case state of
          peDay:
            begin
              StoreValue;
              State := peDaySep;
              if not (xdpTime in Parts) then
                break;
            end;
        else
          RaiseUnexpected(ch);
        end;
      ' ':
        case State of
          peInit,
          peTimeInit,
          peDaySep:
            ; // Ignore leading
          peDay:
            begin
              StoreValue;
              State := peDaySep;
              if not (xdpTime in Parts) then
                break;
            end;
          peMinute, peSecond:
            begin
              StoreValue;
              State := succ(State);
              break;
              // Finish the time at this point.
            end;
        else
          RaiseUnexpected(ch);
        end;
    else
      RaiseUnexpected(ch);
    end;
  end;

  case State of
    peDay, peMinute, peSecond:
      begin
        StoreValue;
        State := Succ(State);
      end;
  end;
  case State of
    peDaySep, peMinuteSep, peFinish:
      begin
        if Parts = [xdpDate, xdpTime] then
          result := EncodeDateTime(yyyy, mm, dd, hh, nn, ss, 0)
        else if Parts = [xdpDate] then
          result := EncodeDate(yyyy, mm, dd)
        else if Parts = [xdpTime] then
          result := EncodeTime(hh,nn,ss,0)
        else
          result := 0;
      end;
  else
    Raise Exception.CreateFmt('Invalid date string: ''%s''', [strVal]);
  end;
end;

const
 CUnixStartDate: TDateTime = 25569.0;
{ Convert from ms since 1/1/1970 to days since 31/12/1899
}
function MSDateToDateTime( msDateTime : int64) : TDateTime;
begin
  result := CUnixStartDate + (msDateTime / 86400000.0);
end;

function DateTimeTomsDate( ADateTime : TDateTime) : Int64;
var
  dblVal : Double;
begin
  dblVal := ADateTime - CUnixStartDate;
  result := Round(dblVal * 86400000);
end;

function ConvertJSONDate(const ADate : String) : TDateTime;
var
  endIDx : integer;
begin
  if StartsText('/Date(', ADate) then
  begin
    endIDx := PosEx(')/', ADate, 7);
    if endIdx = 0 then
      Raise Exception.Create('Unexpected date format: '+ADate);

    if (endIdx > 5) and (ADate[endIdx-5] = '+') then
      Dec(endIdx, 5); // Parse Timezone?

    result := MSDateToDateTime(StrToInt64(Copy(ADate, 7, endIdx-7)));
  end
  else
  begin
    result := XMLStrToDateTime(ADate);
  end;
end;

function DateTimeAsMicrosoftJSONDate(ADateTime : TDateTime) : string;
begin
  result := '/Date('+IntToStr(DateTimeTomsDate(ADateTime)) + ')/';
end;

// Copied from Indy10 : TIdHTTPRequestInfo.DecodeAndSetParams
procedure THTMLParameterStrings.DecodeAndSet( const AValue : String);
var
  i, j : Integer;
  s: string;
begin
  // Convert special characters
  // ampersand '&' separates values    {Do not Localize}
  BeginUpdate;
  try
    Clear;
    i := 1;
    while i <= length(AValue) do
    begin
      j := i;
      while (j <= length(AValue)) and (AValue[j] <> '&') do
      begin
        inc(j);
      end;
      s := copy(AValue, i, j-i);
      // See RFC 1866 section 8.2.1. TP
      s := StringReplace(s, '+', ' ', [rfReplaceAll]);  {do not localize}
      Add(TIdURI.URLDecode(s));
      i := j + 1;
    end;
  finally
    EndUpdate;
  end;
end;

procedure SetSubString(var StrVal : string; Start, Len : integer; const NewString : String);
var
  newlen, diff, strValLen, movelen : Integer;
begin
  strValLen := Length(strVal);
  if len < 0 then
    len := 1+(strValLen- Start);

  newlen := Length(newString);
  diff := newlen- len;
  case diff of
    low(integer)..-1: Delete(StrVal, Start+newlen, -diff);
    0: UniqueString(StrVal);
    1..high(integer):
    begin
      SetLength(StrVal, strValLen + diff);
      movelen :=  1 + strValLen-(start+len);
      if movelen <> 0 then
        Move( strVal[Start+len], strVal[Start+Newlen], movelen*sizeof(Char));
    end;
  end;
  if newlen > 0 then
    Move( newString[1], Strval[start], newLen*sizeof(Char));
end;

function EncodeParamData( const Data : String ) : String;
var
  repStr,
  newData : String;
  idx, last : integer;
  ch : Char;
begin
  newData := TIDURI.ParamsEncode(data);

  idx := 1;
  last := length(newData);
  while idx <= last do
  begin
    ch := newData[idx];
    case ch of
      '?','&',';', '''', '"':
        begin
          repStr := '%'+IntToHex(Ord(ch),2);
          SetSubString(newData, idx, 1, repStr);
          inc(idx, length(repstr));
          inc(last, length(repstr)-1);
        end;
    else Inc(idx);
    end;
  end;
  result := newData;
end;

procedure AddHTMLParam(var parameters : String;const param, Data : String);
var
  newData,
  newParam : String;
begin
  newData := EncodeParamData(Data);

  newParam := param+'='+newData;
  if parameters = '' then
    parameters := newparam
  else
    parameters := Parameters+ '&'+newParam;
end;

procedure SetHTMLParam(parameters : TStrings;const param, Data : String);
var
  newData : String;
begin
  newData := EncodeParamData(Data);
  parameters.Values[param] := newData;
end;

function IncludeTrailingURIDelimeter( const uri : String ) : String;
var
  last : Integer;
begin
  result := uri;
  last := length(result);
  if (last > 0)  and (uri[last] <> '/') then
    result := result + '/';
end;


end.
