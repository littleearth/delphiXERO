unit XERO.Utils;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes;

function IsEmptyString(AValue: string): boolean;

implementation

function IsEmptyString(AValue: string): boolean;
begin
  Result := Trim(AValue) = '';
end;

end.
