unit XERO.API.JSON;

interface

uses
  System.SysUtils, System.Classes, System.Variants, XERO.Types, XERO.API,
  XERO.API.Response.JSON;

type
  TXEROApiJSON = class(TXEROAPIBase)
  private
    FBaseURL: string;
    procedure SetBaseURL(const Value: string);
    function GetURL(AURL: string): string;
  public
    constructor Create(AOwner: TComponent); override;
    function Get(AURL: string; AParams: string; var AResponse: string;
      ALastModified: TDateTime = 0): boolean; reintroduce;
    function Post(AURL: String; ASource: string; var AResponse: string)
      : boolean; reintroduce;
    function Put(AURL: String; ASource: string; var AResponse: string): boolean;
      reintroduce;
    function Delete(AURL: string; var AResponse: string): boolean; reintroduce;

    function Find(AFilter: string = ''; AOrderBy: string = '';
      APage: Integer = 0; ALastModified: TDateTime = 0): string; reintroduce;
    property summarizeErrors;
  published
    property BaseURL: string read FBaseURL write SetBaseURL;

  end;

implementation

{ TXEROApiJSON }

constructor TXEROApiJSON.Create(AOwner: TComponent);
begin
  inherited;
  FBaseURL := XERO_API_BASE_URL;
end;

function TXEROApiJSON.Delete(AURL: string; var AResponse: string): boolean;
var
  LURL: string;
begin
  LURL := GetURL(AURL);
  Result := inherited Delete(LURL, AResponse, rtJSON);
end;

function TXEROApiJSON.Find(AFilter, AOrderBy: string; APage: Integer;
  ALastModified: TDateTime): string;
var
  LURL: string;
  LXEROResponseJSON: TXEROResponseJSON;
begin
  Result := '[]';
  LXEROResponseJSON := TXEROResponseJSON.Create(nil);
  try
    LURL := FBaseURL;
    if inherited Find<TXEROResponseJSON>(LURL, LXEROResponseJSON, AFilter,
      AOrderBy, APage, ALastModified) then
    begin
      Result := LXEROResponseJSON.AsString;
    end
    else
    begin
      raise EXEROException.CreateFmt('Error %d: %s',
        [LXEROResponseJSON.ResponseCode, LXEROResponseJSON.ErrorMessage]);
    end;
  finally
    FreeAndNil(LXEROResponseJSON);
  end;
end;

function TXEROApiJSON.Get(AURL: string; AParams: string; var AResponse: string;
  ALastModified: TDateTime): boolean;
var
  LURL: string;
begin
  LURL := GetURL(AURL);
  Result := inherited Get(LURL, AParams, AResponse, ALastModified, rtJSON);
end;

function TXEROApiJSON.GetURL(AURL: string): string;
begin
  if Pos('http', AURL) = 1 then
  begin
    Result := AURL;
  end
  else
  begin
    Result := FBaseURL + AURL;
  end;
end;

function TXEROApiJSON.Post(AURL: String; ASource: string;
  var AResponse: string): boolean;
var
  LURL: string;
begin
  LURL := GetURL(AURL);
  Result := inherited Post(LURL, '', ASource, AResponse, rtJSON);
end;

function TXEROApiJSON.Put(AURL, ASource: string; var AResponse: string)
  : boolean;
var
  LURL: string;
begin
  LURL := GetURL(AURL);
  Result := inherited Put(LURL, '', ASource, AResponse, rtJSON);
end;

procedure TXEROApiJSON.SetBaseURL(const Value: string);
begin
  FBaseURL := Value;
end;

end.
