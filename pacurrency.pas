Unit paCurrency;
// Use the currency table
// CurrKey : CHAR[3]
// Scale = Decimals

{$mode objfpc}{$H+}

Interface

uses
  Classes, SysUtils;

Type
    TCurrCode=String[3];

Function GetScale(Const CurrCode:TCurrCode):Integer;
Function GetFactor(Const CurrCode:TCurrCode):Integer;
Function FloatToDBAmount(Const CurrCode:TCurrCode; Const Amount:Real):Integer;

implementation

Uses
    Math;

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

//FloatToStrF(Amt,ffCurrency,10,2)

Function GetFactor(Const CurrCode:TCurrCode):Integer;
  var
    Scale:LongInt;
    Base:LongInt=10;
  begin
    Scale := GetScale(CurrCode);
    Result := Base**Scale;
  end;

Function FloatToDBAmount(Const CurrCode:TCurrCode; Const Amount:Real):Integer;
  var
    Scale:Integer;
  begin
    Scale := GetScale(CurrCode);
    If Scale = 0 then
      begin
        Result := Trunc(Amount);
        exit;
      end
    Else
      Result := Trunc(Amount * GetFactor(CurrCode));
  end;

Function DBAmounttoFloat(Const CurrCode:TCurrCode; Const Amount:Integer):Real;
  var
    Scale:Integer;
  begin
    Scale := GetScale(CurrCode);
    If Scale = 0 then
      begin
        Result := Amount;
        exit;
      end
    Else
      Result := Amount / GetFactor(CurrCode);

  end;

end.

