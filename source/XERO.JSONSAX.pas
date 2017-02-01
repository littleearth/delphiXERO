{: SAX Utility wrappers.

  Wrappers around the OXML Library that behave in a similar manner to the SAX
  style parsing of XML.
}
 unit XERO.JSONSax;

interface
uses
  System.Classes, System.Sysutils,
  System.JSON.Readers,
  RTTI;

type

  TJSONEventReader = class;

  { Handler to provide call back for parsing.
  }
  TJSONSaxHandlerBase = class
  protected
    FParser : TJSONEventReader;
  public
    //: Attach a parser to this handler. Ensures FParserEx is set/cleared
    procedure AttachParser( AParser : TJSONEventReader); virtual;

    //: Called when normal pair encountered
    procedure Pair(const FieldName : String; const FieldValue : TValue); virtual;
    //: Called when a value is encountered without a name pair
    procedure Value( const FieldValue : TValue); virtual;

    //: Called at the start of an anonymous object
    procedure StartObject; virtual;
    //: Called when an object pair encountered
    procedure StartObjectPair(const FieldName : String); virtual;
    //: Called at the end of the Object
    procedure EndObject; virtual;

    //: Called when an array pair encountered
    procedure StartArrayPair(const FieldName : String); virtual;
    procedure StartArray; virtual;
    //: Called at the end of an array
    procedure EndArray; virtual;
  end;

  TJsonParseState = (jpsIdle, jpsRunning, jpsPaused, jpsStopping, jpsStopped);

  TJSONEventReader = class
  protected
    FStreamReader : TStreamReader;
    FReader : TJSONTextReader;

    FHandler : TJSONSAXHandlerBase;
    FParseState : TJsonParseState;

    procedure wHandler(NewVal: TJSONSAXHandlerBase);
    function StartParsing : boolean;
    procedure FinishedParsing;

  public
    constructor Create; overload;
    constructor Create(handler : TJSONSAXHandlerBase); overload;
    procedure BeforeDestruction; override;

    function ParseStream(stream : TStream) : boolean;

    procedure ReleaseDocument;
    function ResumeParsing : boolean;

    property Handler: TJSONSAXHandlerBase read FHandler write wHandler;
  end;

  TJSONSAXNestableHandler = class(TJSONSAXHandlerBase)
  protected
    FStarted,
    FEnded : Boolean;
    FOwnNested : boolean;
    FNested : TJSONSAXNestableHandler;
    FEltName : String;
    procedure SetChildElement( handler : TJSONSAXNestableHandler; Owns : Boolean = true);
  public
    constructor Create(AEltName : String);

    procedure Reset;

    //: Called when normal pair encountered
    procedure Pair(const FieldName : String; const FieldValue : TValue); override;
    //: Called when a value is encountered without a name pair
    procedure Value( const FieldValue : TValue); override;

    //: Called at the start of an anonymous object
    procedure StartObject; override;
    //: Called when an object pair encountered
    procedure StartObjectPair(const FieldName : String); override;
    //: Called at the end of the Object
    procedure EndObject; override;

    //: Called when an array pair encountered
    procedure StartArrayPair(const FieldName : String); override;
    procedure StartArray; override;
    //: Called at the end of an array
    procedure EndArray; override;


    {: Start a child element.  This needs to be overridden to call
       SetChildElement with the appropriate handler for the section.
    }
    procedure StartChildElement( const aName: String); virtual;
    {: Start a child array element.  This needs to be overridden to call
       SetChildElement with the appropriate handler for the section.
    }
    procedure StartChildArrayElement( const aName: String); virtual;
    {: End a child array element.
    }
    procedure EndChildArrayElement( const aName: String); virtual;

    {: Start an unnamed child element.  Usually within an array.
    }
    procedure StartChildObject; virtual;
    {: Finishing a child element (before destruct)
    }
    procedure EndingChildObject; virtual;
    {: Finish a child element.
    }
    procedure EndChildObject; virtual;

    //: Start this element
    procedure StartMyElement; virtual;
    //: Called at the finish of this element
    procedure EndMyElement; virtual;

    //: Called when normal pair encountered
    procedure MyPair(const FieldName : String; const FieldValue : TValue); virtual;
    //: Called when a value is encountered without a name pair
    procedure MyValue( const FieldValue : TValue); virtual;

    property EltName: String read FEltName write FEltName;
    //: Has this element been started
    property Started: Boolean read FStarted;
    //: Has this element been ended
    property Ended: Boolean read FEnded;
  end;

implementation

uses
  System.JSON.Types;

// TJSONSaxHandlerBase
//

// public definitions

// Attach a parser to this handler. Ensures FParserEx is set/cleared
procedure TJSONSaxHandlerBase.AttachParser( AParser : TJSONEventReader);
begin
  if assigned(FParser) then
    FParser.wHandler(nil);

  FParser := AParser;

  if assigned(FParser) then
    FParser.wHandler(self);
end;

// Called when normal pair encountered
procedure TJSONSaxHandlerBase.Pair(const FieldName : String; const FieldValue : TValue);
begin
end;

procedure TJSONSaxHandlerBase.Value( const FieldValue : TValue);
begin
end;

// Called at the start of an anonymous object
procedure TJSONSaxHandlerBase.StartObject;
begin
end;

// Called when an object pair encountered
procedure TJSONSaxHandlerBase.StartObjectPair(const FieldName : String);
begin
end;

// Called at the end of the Object
procedure TJSONSaxHandlerBase.EndObject;
begin
end;

// Called when an array pair encountered
procedure TJSONSaxHandlerBase.StartArrayPair(const FieldName : String);
begin
end;
procedure TJSONSaxHandlerBase.StartArray;
begin
end;

// Called at the end of an array
procedure TJSONSaxHandlerBase.EndArray;
begin
end;

// TJSONEventReader
//

// protected definitions

procedure TJSONEventReader.wHandler(NewVal: TJSONSAXHandlerBase);
var
  oldHandler : TJSONSAXHandlerBase;
begin
  if newVal <> FHandler then
  begin
    oldHandler := FHandler;
    FHandler := nil; // Prevent AttachParser recursing.
    if assigned(oldHandler) then
      oldHandler.AttachParser(nil);
    FHandler := newVal;
    if assigned(FHandler) then
      FHandler.AttachParser(self);
  end;
end;

// public definitions

constructor TJSONEventReader.Create;
begin

  FParseState := jpsIdle;
  inherited Create;
end;

constructor TJSONEventReader.Create(handler : TJSONSAXHandlerBase);
begin
  FParseState := jpsIdle;
  wHandler(handler);
  inherited Create;
end;

procedure TJSONEventReader.BeforeDestruction;
begin
  ReleaseDocument;
  wHandler(nil);
  inherited;
end;

function TJSONEventReader.ParseStream(stream : TStream) : boolean;
begin
  if Assigned(FReader) then
    ReleaseDocument;
  FParseState := jpsIdle;
  FStreamReader := TStreamReader.Create(stream);
  FReader := TJSONTextReader.Create(FStreamReader);

  result := StartParsing;
end;

procedure TJSONEventReader.ReleaseDocument;
begin
  FreeAndNil(FReader);
  FreeAndNil(FStreamReader);
end;

procedure TJSONEventReader.FinishedParsing;
begin
  if not (fParseState in [jpsIdle, jpsStopping]) then
  begin
    if fParseState = jpsStopping then
      fParseState := jpsStopped
    else
      fParseState := jpsIdle;
  end;
end;

function TJSONEventReader.StartParsing : boolean;
begin
  if not (fParseState in [jpsIdle]) then
    Raise Exception.Create('Unable to start parsing');
  fParseState := jpsPaused;
  result := ResumeParsing;
end;

function TJSONEventReader.ResumeParsing : boolean;
var
  canPause : boolean;
  foundName : boolean;
  curName : String;
begin
  if not (fParseState in [jpsPaused]) then
    raise Exception.Create('Parsing isn''t paused: unable to resume');

  if not assigned(FHandler) then
    raise Exception.Create('Handler must be assigned');

  FParseState := jpsRunning;

  result := true;
  foundName := false;
  curName := '';

  while FReader.Read do
  begin
    canPause := true;
    if not assigned(FHandler) then
    begin
      FParseState := jpsStopping;
      break;
    end;
    case FReader.TokenType of
      TJsonToken.StartObject:
        if not foundName then
          FHandler.StartObject
        else
        begin
          FHandler.StartObjectPair(curName);
          foundName := false;
        end;
      TJsonToken.EndObject:
        begin
          FHandler.EndObject;
          foundName := false;
        end;
      TJsonToken.StartArray:
        if not foundName then
          FHandler.StartArray
        else
        begin
          FHandler.StartArrayPair(curName);
          foundName := false;
        end;
      TJsonToken.EndArray:
        FHandler.EndArray;
      TJsonToken.PropertyName:
        begin
          canPause := false;
          foundName := true;
          curName := FReader.Value.AsString;
        end;
      TJsonToken.Integer,
      TJsonToken.Float,
      TJsonToken.String,
      TJsonToken.Boolean,
      TJsonToken.Date,
      TJsonToken.Null,
      TJsonToken.Bytes:
        begin
          if not foundName then
            FHandler.Value(FReader.Value)
          else
          begin
            FHandler.Pair( curName, FReader.Value);
            FoundName := false;
          end;
        end;
    end;

    case FParseState of
      jpsPaused: if canPause then break;
      jpsStopping: break;
    end;
  end;

  case FParseState of
    jpsStopping: FinishedParsing;
    jpsRunning:
      FinishedParsing;
  end;

end;


// TJSONSAXNestableHandler
//
//
constructor TJSONSAXNestableHandler.Create(AEltName : String);
begin
  FEltName := AEltName;
  inherited Create;
end;

procedure TJSONSAXNestableHandler.Reset;
begin
  FStarted := false;
  FEnded := false;
end;

// protected definitions

procedure TJSONSAXNestableHandler.SetChildElement( handler : TJSONSAXNestableHandler; Owns : Boolean = true);
begin
  if assigned(FNested) then
    raise Exception.CreateFmt('Previous child element ''%s'' still open', [FNested.EltName]);
  FOwnNested := Owns;
  FNested := handler;
  FNested.Reset;
  FNested.StartObject;
end;

// public definitions

// Called when normal pair encountered
procedure TJSONSAXNestableHandler.Pair(const FieldName : String; const FieldValue : TValue);
begin
  if assigned(FNested) then
    FNested.Pair(FieldName, fieldValue)
  else
    MyPair(FieldName, FieldValue);
end;

// Called when a value is encountered without a name pair
procedure TJSONSAXNestableHandler.Value( const FieldValue : TValue);
begin
  if assigned(FNested) then
    FNested.Value( fieldValue)
  else
    MyValue(fieldValue);
end;

// Called at the start of an anonymous object
procedure TJSONSAXNestableHandler.StartObject;
begin
  if not FEnded then
  begin
    if not FStarted then
    begin
      FStarted := true;
      StartMyElement;
    end
    else if assigned(FNested) then
      FNested.StartObject
    else
      StartChildObject;
  end;
end;

// Called when an object pair encountered
procedure TJSONSAXNestableHandler.StartObjectPair(const FieldName : String);
begin
  if not FEnded then
  begin
    if not FStarted then
    begin
      FStarted := true;
      StartMyElement;
    end
    else if assigned(FNested) then
      FNested.StartObjectPair(FieldName)
    else
      startChildElement(FieldName);
  end;
end;

// Start this element
procedure TJSONSAXNestableHandler.StartMyElement;
begin
end;

// Called at the finish of this element
procedure TJSONSAXNestableHandler.EndMyElement;
begin
end;

// Called at the end of the Object
procedure TJSONSAXNestableHandler.EndObject;
begin
  if FStarted and not FEnded then
  begin
    if assigned(FNested) then
    begin
      FNested.EndObject;
      if FNested.Ended then
      begin
        EndingChildObject;
        if FOwnNested then
          FreeAndNil(FNested)
        else
          FNested := nil;
        EndChildObject;
      end;
    end
    else
    begin
      FEnded := true;
      EndMyElement;
    end;
  end;
end;

// Called when an array pair encountered
procedure TJSONSAXNestableHandler.StartArrayPair(const FieldName : String);
begin
  if not FEnded then
  begin
    if not FStarted then
    begin
      FStarted := true;
      StartMyElement;
    end
    else if assigned(FNested) then
      FNested.StartArrayPair(FieldName)
    else
      StartChildArrayElement(FieldName);
  end;
end;

procedure TJSONSAXNestableHandler.StartArray;
begin
  if not FEnded then
  begin
    if not FStarted then
    begin
      FStarted := true;
      StartMyElement;
    end
    else if assigned(FNested) then
      FNested.StartArray
    else
      StartChildArrayElement('');
  end;
end;

// Called at the end of an array
procedure TJSONSAXNestableHandler.EndArray;
begin
  if FStarted and not FEnded then
  begin
    if assigned(FNested) then
    begin
      FNested.EndArray;
      if FNested.Ended then
      begin
        EndChildArrayElement(FNested.EltName);
        if FOwnNested then
          FreeAndNil(FNested)
        else
          FNested := nil;
      end;
    end
    else
    begin
      FEnded := true;
      EndMyElement;
    end;
  end;
end;

{ Start a child element.  This needs to be overridden to call
SetChildElement with the appropriate handler for the section.
}
procedure TJSONSAXNestableHandler.StartChildElement( const aName: String);
begin
  SetChildElement(TJSONSAXNestableHandler.Create(aName));
end;

procedure TJSONSAXNestableHandler.StartChildArrayElement( const aName: String);
begin
  SetChildElement(TJSONSAXNestableHandler.Create(aName));
end;

{ End a child array element.
}
procedure TJSONSAXNestableHandler.EndChildArrayElement( const aName: String);
begin
end;

{ Start an unnamed child element.  Usually within an array.
}
procedure TJSONSAXNestableHandler.StartChildObject;
begin
  SetChildElement(TJSONSAXNestableHandler.Create(''));
end;

{ Finishing a child element (before destruct)
}
procedure TJSONSAXNestableHandler.EndingChildObject;
begin
end;
procedure TJSONSAXNestableHandler.EndChildObject;
begin
end;

// Called when normal pair encountered
procedure TJSONSAXNestableHandler.MyPair(const FieldName : String; const FieldValue : TValue);
begin
end;

// Called when a value is encountered without a name pair
procedure TJSONSAXNestableHandler.MyValue( const FieldValue : TValue);
begin
end;

end.
