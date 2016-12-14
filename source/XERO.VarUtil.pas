{: Variant utilities
 @cat
}
unit XERO.VarUtil;

interface
uses Classes, Sysutils;

function CheckCastVarAsBoolean( varvalue : Variant; Var BoolVal, BadType : Boolean; AllowNull : boolean  ) : boolean;
function CastVarAsBoolean( varvalue : Variant; defValue, AllowNull, AllowBadType : boolean) : boolean;
Function CastVarAsString( varValue : Variant; ForDisplay : Boolean = false ) : String;
Function CastVarAsInt( varValue : Variant ): LongInt;
Function CastVarAsDouble( varValue : Variant ): double;
function CastVarAsCurrency( VarValue : Variant) : Currency;
Function CastVarAsDateTime( varValue : Variant ): TDateTime;
function VarIsInteger( VarValue : Variant) : boolean;
function VarIsRealNumber( VarValue : Variant) : boolean;
function SanitiseVariant( const varValue : Variant ) : string; overload;
function SanitiseVariant( const varValue : Variant; alwaysQuote : boolean ) : string; overload;

implementation

uses Variants, StrUtils, XERO.MiscUtil;

function CheckCastVarAsBoolean( varvalue : Variant; Var BoolVal, BadType : Boolean; AllowNull : boolean  ) : boolean;
var
  StrVal : String;
begin
  BadType := false;
  case VarType(varvalue) of
    varSingle, varDouble, varCurrency:
    begin
      BoolVal := (varvalue <> 0.0);
      result := not BoolVal or (VarValue = 1) or (VarValue = -1);
    end;
    varLongWord, varInt64, varByte, varWord, varShortInt,
    varSmallInt, varInteger:
      begin
        BoolVal := (varvalue <> 0);
        result := not BoolVal or (VarValue = 1) or (VarValue = -1);
      end;
    varEmpty, varNull:
      begin
        BoolVal := false;
        result := AllowNull;
      end;
    {$IFDEF UNICODE}varUString,{$ENDIF}
    varOleStr, varString, varStrArg:
    begin
      strval := varvalue;
      case IndexText(Strval, ['no','n','0','false','f', 'yes','y','1','true','t']) of
        0..4 :
        begin
          result := true;
          BoolVal := false;
        end;
        5..9 :
        begin
          result := true;
          BoolVal := true;
        end;
      else
        BoolVal := false;
        result := false;
      end;
    end;
    varBoolean:
      begin
        result := true;
        BoolVal := varvalue;
      end;
  else
    result := false;
    BoolVal := false;
    BadType := true;
  end;
end;

function SanitiseString( const strval : String; doRightTrim : boolean; alwaysQuote : boolean = false) : string;
var
  idx : integer;
  atEnd,badEnd,hasCtl,inCtl : boolean;
begin
  result := strval;
  inCtl := false;
  atEnd := true;
  badEnd := false;
  hasCtl := alwaysQuote;
  // From end to beginning  - so that we can strip spaces.
  for idx := length(result) downto 1 do
    case ord(result[idx]) of
    0..31:
      begin
        hasCtl := true;
        if not inCtl and not atEnd then
          insert( '''', result,  idx+1);
        insert( IntToStr(Ord(result[idx])), result,idx+1);
        result[idx] := '#';
        inCtl := true;
        if atEnd then
        begin
          badEnd := true;
          atEnd := false;
        end;
      end;
    32:
      begin
        if not DoRightTrim then
          atEnd := false
        else if atEnd then delete(result,idx,1);
        if inCtl then
        begin
          Insert('''', result, idx+1);
          inCtl := false;
        end;
      end;

    else
      atEnd := false;
      if inCtl then
        Insert('''', result, idx+1);
      inCtl := false;
    end;
  if hasCtl then
  begin
    if not badEnd then result := result + '''';
    if not inCtl then result :=  '''' + result;
  end;
end;


function SanitiseVariant( const varValue : Variant; alwaysQuote : boolean ) : string;
var
  dt : TDateTime;
  dbl : double;
  idx : integer;
  function SafeCastVarAsString( const varValue : Variant) : string;
  begin
    try
      case VarType(varValue) of
           varEmpty, varNull,
           varLongWord, varInt64, varByte, varWord, varShortInt,
           varSmallint, varInteger, varSingle, varDouble, varCurrency,
           varOleStr, varStrArg, varString,{$IFDEF UNICODE}varUString,{$ENDIF}
           varDate,
           varBoolean,
           varArray or varByte:
                    result := CastVarAsString(varValue);
      else
        result := string(varValue);
      end;
    except
      result := '(invalid variant)';
    end;
  end;
  Function Zero(const R : Double) : Boolean;
  begin
    Result := (R <= 0.00009) and (R >= 0.00009);
  end;
begin
  case VarType(varValue) of
    varEmpty: result := '(empty)';
    varNull:  result := '(null)';
    varOleStr, varStrArg, {$IFDEF UNICODE}varUString,{$ENDIF}varString: result := SanitiseString(CastVarAsString(varValue), true, alwaysQuote);
    varArray or varByte:
      result := SanitiseString(SafeCastVarAsString(varValue), false, alwaysQuote);

    varArray or varString, varArray or varStrArg, varArray or varOleStr{$IFDEF UNICODE}, varArray or varUString{$ENDIF}:
      begin
        result := '{';
        for idx := VarArrayLowBound(VarValue, 1) to VarArrayHighBound(VarValue, 1) do
          result := result + ',' + SanitiseString( VarValue[idx], true, alwaysQuote);
        if length(result) >= 2 then
          result[2] := '{';
        result := result + '}}';
      end;
    varArray or varVariant:
      begin
        result := '{';
        for idx := VarArrayLowBound(VarValue, 1) to VarArrayHighBound(VarValue, 1) do
          result := result + ',' + SanitiseVariant(VarValue[idx]);
        if length(result) >= 2 then
          result[2] := '{';
        result := result + '}}';
      end;

    varDate:
    begin
      dt := varValue;
      dbl := dt;
      if dbl = 0. then
        result := '(zero)'
      else if trunc(dbl) = 0 then // Time
        result := FormatDateTime('hh:nn:ss.zz',dt)
      else if dbl = 922337203685477 then
        result := '(max)'
      else
      begin
        dbl := dbl-(trunc(dbl));
        if Zero(dbl) then // Date
          result := FormatDateTime('dmmmyyyy',dt)
        else if dbl = 922337203685477 then
          result := '(max)'
        else
          result := FormatDateTime('hh:nn:ss.zz dmmmyyyy',dt);
      end;
    end;
  else
    try
      result := String(varValue);
    except
      result := '(invalid variant)';
    end;
  end;
end;
function SanitiseVariant( const varValue : Variant ) : string;
begin
  result := SanitiseVariant(varValue, true);
end;


function CastVarAsBoolean( varvalue : Variant; defValue, AllowNull, AllowBadType : boolean) : boolean;
var
  BadType : boolean;
begin
  if not CheckCastVarAsBoolean(varvalue, result, BadType, AllowNull) then
  begin
    if (not BadType) or AllowBadType then
      result := defValue
    else
      raise SysUtils.EConvertError.Create('Unable to cast '+SanitiseVariant(varvalue)+ ' as Boolean');
  end;
end;


// Convert a variant to a string.
Function CastVarAsString( varValue : Variant; ForDisplay : Boolean = false ) : String;
const
  CBoolToDisp : Array[false..true] of array [false..true] of string = (('N', 'Y'),('No', 'Yes'));
var
  dbl: Double;
  Len : integer;
  Hour, Min, Sec, MSec : word;
  truncval : int64;
  StrPtr   : Pointer;
begin
  case VarType(varValue) of
    varEmpty, varNull:
      result := '';
    varLongWord, varInt64, varWord, varShortInt,
    varSmallint, varInteger:
      begin
        truncVal := varValue;
        result := IntToStr(truncVal);
      end;
    varSingle, varDouble, varCurrency,  varByte,
    varOleStr, varStrArg, {$IFDEF UNICODE}varUString,{$ENDIF}varString:
      result := varValue;
    varDate:
    begin
      dbl := varValue;
      if dbl = 0. then
        result := ''
      else
      begin
        truncval := trunc(Dbl);
        if truncval = 0 then // Time
        begin
          if ForDisplay then
            result := FormatDateTime('h:mm:ss am/pm', varValue)
          else
          begin
            DecodeTime(varValue, Hour, Min, Sec, MSec);
            Result := Format('%02.2d%02.2d%02.2d%02.2d',[Hour, min, sec, Round(Msec/10)]);
          end;
        end
        else if (dbl-truncval) = 0 then // Date
        begin
          if ForDisplay then
            result := formatDateTime('dd/mm/yyyy', varValue)
          else
          begin
            result := FormatDateTime('yyyy.mm.dd', TDateTime(varValue));
          end;
        end
        else
        begin
          if ForDisplay then
            result := formatDateTime('h:mm:ss am/pm dd/mm/yyyy', varValue)
          else
          begin
            result := FormatDateTime('hh:mm:ss yyyy.mm.dd', TDateTime(varValue));
          end;
        end;
      end;
    end;
    varBoolean:
      result := CBoolToDisp[ForDisplay][boolean(varValue)];
    varArray or varByte:
    begin
      Len := 1+VarArrayHighBound(VarValue, 1) - VarArrayLowBound(VarValue, 1);
      if Len = 0 then
        result := ''
      else
      begin
        SetLength(Result, Len);
        StrPtr := VarArrayLock(varValue);
        try
          move( StrPtr^, Result[1],Len);
        finally
          VarArrayUnlock(varValue);
        end;
      end;
    end;
  else
    Raise SysUtils.EConvertError.CreateFmt('Unable to cast %s as a string',[SanitiseVariant(varValue)]);
  end;
end;

Function CastVarAsInt( varValue : Variant ): LongInt;
var
  val64 : int64;
begin
  Case VarType(varValue) of
    varSingle, varDouble, varCurrency: result := Trunc(varValue);
    varLongWord, varInt64:
      begin
        val64 := varValue;
        if (val64 < low(longInt)) or (val64 > high(longint)) then
          raise SysUtils.EConvertError.Create('Unable to cast '+SanitiseVariant(varvalue)+ ' as an Integer');
        result := LongInt(varValue);
      end;
    varByte, varWord,
    varShortInt,
    varSmallInt, varInteger:           result := Integer(varValue);
    varEmpty, varNull:                 result := 0;
    {$IFDEF UNICODE}varUString,{$ENDIF}
    varOleStr, varString, varStrArg:   result := StrToInt(String(varValue));
    varDate:                           result := Trunc(varValue);
    varBoolean: if varValue then result := 1 else result := 0;
  else
    raise SysUtils.EConvertError.Create('Unable to cast '+SanitiseVariant(varvalue)+ ' as an Integer');
  end;
end;

// Convert a variant to a string.
Function CastVarAsDouble( varValue : Variant ): double;
begin
  Case VarType(varValue) of
    varSingle, varDouble, varCurrency: result := varValue;
    varLongWord, varInt64, varByte, varWord, varShortInt,
    varSmallInt, varInteger:           result := varValue;
    varEmpty, varNull:                 result := 0.0;
    {$IFDEF UNICODE}varUString,{$ENDIF}
    varOleStr, varString, varStrArg:   result := StrToFloat(varValue);
    varDate:                           result := varValue;
    varBoolean: if varValue then result := 1.0 else result := 0.0;
  else
    raise SysUtils.EConvertError.Create('Unable to cast '+SanitiseVariant(varvalue)+ ' as a floating value');
  end;
end;

function CastVarAsCurrency( VarValue : Variant) : Currency;
var
  DoubleVal : Double;
begin
  Case VarType(varValue) of
    varSingle, varCurrency: result := varValue;
    varDouble:
      try
        DoubleVal := Double(VarValue);
        DoubleVal := DoubleVal + 0.00004;
        result := DoubleVal;
      except
        on E:SysUtils.EConvertError do
          raise SysUtils.EConvertError.Create('Unable to cast '+SanitiseVariant(varvalue)+ ' as a Currency: ' + E.Message);
      end;
    varLongWord, varInt64, varByte, varWord, varShortInt,
    varSmallInt, varInteger:           result := varValue;
    varEmpty, varNull:                 result := 0.0;
    {$IFDEF UNICODE}varUString,{$ENDIF}
    varOleStr, varString, varStrArg:
      try
        result := StrToCurr(varValue);
      except
        on E:SysUtils.EConvertError do
          raise SysUtils.EConvertError.Create('Unable to cast '+SanitiseVariant(varvalue)+ ' as a Currency: ' + E.Message);
      end;
    varBoolean: if varValue then result := 1.0 else result := 0.0;
  else
    raise SysUtils.EConvertError.Create('Unable to cast '+SanitiseVariant(varvalue)+ ' as a Currency');
  end;
end;
Function CastVarAsDateTime( varValue : Variant ): TDateTime;
begin
  Case VarType(varValue) of
    varSingle, varDouble, varCurrency: result := varValue;
    varLongWord, varInt64, varByte, varWord, varShortInt,
    varSmallInt, varInteger:           result := varValue;
    varEmpty, varNull:                 result := 0;
    {$IFDEF UNICODE}varUString,{$ENDIF}
    varOleStr, varString, varStrArg:
      result := XMLStrToDateTime( varValue, [xdpDate, xdpTime]);
    varDate:                           result := varValue;
  else
    raise SysUtils.EConvertError.Create('Unable to cast '+SanitiseVariant(varvalue)+ ' as a Date');
  end;
end;


function VarIsInteger( VarValue : Variant) : boolean;
begin

  case VarType(VarValue) of
    varLongWord, varInt64, varByte, varWord, varShortInt, varSmallInt,varInteger:
      result := true;
  else result := false;
  end;
end;

function VarIsRealNumber( VarValue : Variant) : boolean;
begin
  case VarType(VarValue) of
    varSingle, varDouble, varCurrency:
      result := true;
  else result := false;
  end;
end;

end.

