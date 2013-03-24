unit paCurrency;
// Use the currency table
// CurrKey : CHAR[3]
// Scale = Decimals

{$mode objfpc}{$H+}

interface


uses
  Classes, SysUtils;

Type
    TCurrCode=String[3];

Function GetScale(const CurrCode:TCurrCode):Integer;


implementation

Function GetScale(const CurrCode:TCurrCode):Integer;
  begin
// Later we'll read from the database
//    Select Decimals from Currency where CurrKey = CurrCode:
// For now we'll just hard-code it.
    If CurrCode = 'JPY' then
      Result := 0
    else   // USD, CAD, Euro, etc.
      Result := 2;
  end;

end.

