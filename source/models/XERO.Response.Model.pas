unit XERO.Response.Model;

interface

uses
  Classes, SysUtils, XERO.Model;

type
  TXEROApiExceptionValidationError = class(TXEROModel)
  private
    FMessage: string;
    procedure SetMessage(const Value: string);
  public
    property Message: string read FMessage write SetMessage;
  end;

  TXEROApiExceptionValidationErrors =
    TXEROModelList<TXEROApiExceptionValidationError>;

  TXEROApiExceptionElement = class(TXEROModel)
  private
    [XEROModelManagedAttribute]
    FValidationErrors: TXEROApiExceptionValidationErrors;
  public
    function GetDetailedMessage: string;
    property ValidationErrors: TXEROApiExceptionValidationErrors
      read FValidationErrors;
  end;

  TXEROApiExceptionElements = TXEROModelList<TXEROApiExceptionElement>;

  TXEROApiException = class(TXEROModel)
  private
    [XEROModelManagedAttribute]
    FElements: TXEROApiExceptionElements;
    FMessage: string;
    FErrorNumber: integer;
    FErrorType: string;

    procedure SetErrorNumber(const Value: integer);
    procedure SetErrorType(const Value: string);
    procedure SetMessage(const Value: string);
  public
    function GetDetailedMessage: string;
    property ErrorNumber: integer read FErrorNumber write SetErrorNumber;
    [XEROModelJSONPropertyNameAttribute('Type')]
    property ErrorType: string read FErrorType write SetErrorType;
    property Message: string read FMessage write SetMessage;
    property Elements: TXEROApiExceptionElements read FElements;
  end;

  TXEROResponse = class(TXEROModel)
  private
    [XEROModelManagedAttribute]
    FApiException: TXEROApiException;
    FProviderName: string;
    FID: string;
    FStatus: string;
    FDateTimeUTC: string;
    FResponseCode: integer;
    FErrorMessage: string;
    procedure SetDateTimeUTC(const Value: string);
    procedure SetID(const Value: string);
    procedure SetProviderName(const Value: string);
    procedure SetStatus(const Value: string);
    procedure SetErrorMessage(const Value: string);
    procedure SetResponseCode(const Value: integer);
    function GetErrorMessage: string;
  public
    function IsSuccess: boolean;
    property ID: string read FID write SetID;
    property Status: string read FStatus write SetStatus;
    property ProviderName: string read FProviderName write SetProviderName;
    property DateTimeUTC: string read FDateTimeUTC write SetDateTimeUTC;
    property ResponseCode: integer read FResponseCode write SetResponseCode;
    property ErrorMessage: string read GetErrorMessage write SetErrorMessage;
    property ApiException: TXEROApiException read FApiException;
  end;

implementation

uses
  XERO.Utils;

{ TXEROResponseModel }

function TXEROResponse.GetErrorMessage: string;
begin
  if (ResponseCode = 400) and Assigned(FApiException) then
  begin
    Result := FApiException.GetDetailedMessage;
    if IsEmptyString(Result) then
      Result := FErrorMessage;
  end
  else
  begin
    Result := FErrorMessage;
  end;

end;

function TXEROResponse.IsSuccess: boolean;
begin
  Result := SameText('OK', Status);
end;

procedure TXEROResponse.SetDateTimeUTC(const Value: string);
begin
  FDateTimeUTC := Value;
end;

procedure TXEROResponse.SetErrorMessage(const Value: string);
begin
  FErrorMessage := Value;
end;

procedure TXEROResponse.SetID(const Value: string);
begin
  FID := Value;
end;

procedure TXEROResponse.SetProviderName(const Value: string);
begin
  FProviderName := Value;
end;

procedure TXEROResponse.SetResponseCode(const Value: integer);
begin
  FResponseCode := Value;
end;

procedure TXEROResponse.SetStatus(const Value: string);
begin
  FStatus := Value;
end;

{ TXEROApiExceptionValidationError }

procedure TXEROApiExceptionValidationError.SetMessage(const Value: string);
begin
  FMessage := Value;
end;

{ TXEROApiException }

function TXEROApiException.GetDetailedMessage: string;
var
  LElement: TXEROApiExceptionElement;
begin
  if not IsEmptyString(Self.ErrorType) then
  begin
    Result := Format('[%d] %s %s', [Self.ErrorNumber, Self.ErrorType,
      Self.Message]);
    if Assigned(FElements) then
    begin
      for LElement in FElements do
      begin
        Result := Result + LElement.GetDetailedMessage;
      end;
    end;
  end;
end;

procedure TXEROApiException.SetErrorNumber(const Value: integer);
begin
  FErrorNumber := Value;
end;

procedure TXEROApiException.SetErrorType(const Value: string);
begin
  FErrorType := Value;
end;

procedure TXEROApiException.SetMessage(const Value: string);
begin
  FMessage := Value;
end;

{ TXEROApiExceptionElement }

function TXEROApiExceptionElement.GetDetailedMessage: string;
var
  LValidationError: TXEROApiExceptionValidationError;
begin
  Result := '';
  for LValidationError in FValidationErrors do
  begin
    if not IsEmptyString(Result) then
      Result := Result + #13#10;
    Result := Result + LValidationError.Message;
  end;
end;

end.
