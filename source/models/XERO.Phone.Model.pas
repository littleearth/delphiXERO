unit XERO.Phone.Model;

interface

uses
  Classes, SysUtils, XERO.Model;

type
  TXMPhone = class(TXeroModel)
  private
    FPhoneNumber: string;
    FPhoneType: string;
    FPhoneCountryCode: string;
    FPhoneAreaCode: string;
    procedure SetPhoneAreaCode(const Value: string);
    procedure SetPhoneCountryCode(const Value: string);
    procedure SetPhoneNumber(const Value: string);
    procedure SetPhoneType(const Value: string);
  public
    property PhoneType: string read FPhoneType write SetPhoneType;
    property PhoneNumber: string read FPhoneNumber write SetPhoneNumber;
    property PhoneAreaCode: string read FPhoneAreaCode write SetPhoneAreaCode;
    property PhoneCountryCode: string read FPhoneCountryCode
      write SetPhoneCountryCode;
  end;

type
  TXMPhones = TXEROModelList<TXMPhone>;

implementation

{ TXMPhone }

procedure TXMPhone.SetPhoneAreaCode(const Value: string);
begin
  FPhoneAreaCode := Value;
end;

procedure TXMPhone.SetPhoneCountryCode(const Value: string);
begin
  FPhoneCountryCode := Value;
end;

procedure TXMPhone.SetPhoneNumber(const Value: string);
begin
  FPhoneNumber := Value;
end;

procedure TXMPhone.SetPhoneType(const Value: string);
begin
  FPhoneType := Value;
end;

end.
