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
    function ParamsToURL(AURL, AParams: string): string;
  public

    constructor Create(AOwner: TComponent); override;
    function Get(AURL: string; AParams: string; ALastModified: TDateTime = 0)
      : string; reintroduce;
    function Post(AURL: String; ASource: string; var AResponse: string;
      var AResponseCode: integer): Boolean; reintroduce;
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

function TXEROApiJSON.Get(AURL, AParams: string;
  ALastModified: TDateTime): string;
var
  LResponse: string;
  LURL: string;
begin
  LURL := ParamsToURL(FBaseURL + AURL, AParams);
  Result := '[]';
  if inherited Get(LURL, '', LResponse, ALastModified, rtJSON) then
  begin
    Result := LResponse;
  end;
end;

function TXEROApiJSON.ParamsToURL(AURL, AParams: string): string;
begin
  Result := AURL + '?' + AParams;
end;

function TXEROApiJSON.Post(AURL: String; ASource: string; var AResponse: string;
  var AResponseCode: integer): Boolean;
var
  LResponse: TStringStream;
  LErrorMessage: string;
  LURL: string;
  LSource: TStringList;
begin
  Result := False;
  LSource := TStringList.Create;
  LResponse := TStringStream.Create('');
  try
    LURL := FBaseURL + AURL;
    LSource.Text := ASource;
    LResponse.Position := 0;
    if not inherited Post(LURL, LSource, LResponse, AResponseCode,
      LErrorMessage, rtJSON) then
    begin
      AResponse := LResponse.ReadString(LResponse.Size);
      Result := TRue;
    end
    else
    begin
      AResponse := LErrorMessage;
    end;
  finally
    FreeAndNil(LResponse);
    FreeAndNil(LSource);
  end;
end;

procedure TXEROApiJSON.SetBaseURL(const Value: string);
begin
  FBaseURL := Value;
end;

end.
