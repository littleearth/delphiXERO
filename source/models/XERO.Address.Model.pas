unit XERO.Address.Model;

interface

uses
  Classes, SysUtils, XERO.Model;

type
  TXMAddress = clasS(TXeroModel)
  private
    FPostalCode: string;
    FAddressLine2: string;
    FAddressLine3: string;
    FAddressLine1: string;
    FAddressLine4: string;
    FAddressType: string;
    FCountry: string;
    FCity: string;
    FRegion: string;
    FAttentionTo: string;
    procedure SetAddressLine1(const Value: string);
    procedure SetAddressLine2(const Value: string);
    procedure SetAddressLine3(const Value: string);
    procedure SetAddressLine4(const Value: string);
    procedure SetAddressType(const Value: string);
    procedure SetAttentionTo(const Value: string);
    procedure SetCity(const Value: string);
    procedure SetCountry(const Value: string);
    procedure SetPostalCode(const Value: string);
    procedure SetRegion(const Value: string);
  public
    property AddressType: string read FAddressType write SetAddressType;
    property AddressLine1: string read FAddressLine1 write SetAddressLine1;
    property AddressLine2: string read FAddressLine2 write SetAddressLine2;
    property AddressLine3: string read FAddressLine3 write SetAddressLine3;
    property AddressLine4: string read FAddressLine4 write SetAddressLine4;
    property City: string read FCity write SetCity;
    property Region: string read FRegion write SetRegion;
    property PostalCode: string read FPostalCode write SetPostalCode;
    property Country: string read FCountry write SetCountry;
    property AttentionTo: string read FAttentionTo write SetAttentionTo;
  end;

type
  TXMAddresses = TXEROModelList<TXMAddress>;

implementation

{ TXMAddress }

procedure TXMAddress.SetAddressLine1(const Value: string);
begin
  FAddressLine1 := Value;
end;

procedure TXMAddress.SetAddressLine2(const Value: string);
begin
  FAddressLine2 := Value;
end;

procedure TXMAddress.SetAddressLine3(const Value: string);
begin
  FAddressLine3 := Value;
end;

procedure TXMAddress.SetAddressLine4(const Value: string);
begin
  FAddressLine4 := Value;
end;

procedure TXMAddress.SetAddressType(const Value: string);
begin
  FAddressType := Value;
end;

procedure TXMAddress.SetAttentionTo(const Value: string);
begin
  FAttentionTo := Value;
end;

procedure TXMAddress.SetCity(const Value: string);
begin
  FCity := Value;
end;

procedure TXMAddress.SetCountry(const Value: string);
begin
  FCountry := Value;
end;

procedure TXMAddress.SetPostalCode(const Value: string);
begin
  FPostalCode := Value;
end;

procedure TXMAddress.SetRegion(const Value: string);
begin
  FRegion := Value;
end;

end.
