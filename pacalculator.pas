unit paCalculator;

{$mode objfpc}{$H+}

interface

 uses
  Classes, SysUtils, libpa;

 type
   // Single Currency DrCr Calculator
   TDrCrCalculator  = class (Tobject)
     private
       _flatBal:Integer; // flat single number balance
                         // +:dr, -:Cr
       _Curr:TCurrCode; // Holds the currency we are using.
     public
       Constructor Create(Currency:TCurrCode);
       Procedure clear;
       Procedure AddEntry(amt:integer; DrCr:TDrCr; Curr:TCurrCode);
       Function Balance:Integer;
       Function DrCr:TDrCr;
   end;



implementation

  constructor TDrCrCalculator.Create(Currency:TCurrCode);
    begin
      _flatbal := 0;
      _Curr := Currency;
    end;

  procedure TDrCrCalculator.clear;
    begin
      _flatBal := 0;
    end;

  Procedure TDrCrCalculator.AddEntry(amt:integer; DrCr:TDrCr; Curr:TCurrCode);
    var
      FlatAmt:Integer;
    begin
      // This object can only handle one kind of currency,
      // if they try to add other types, get angry
      if Curr <> _Curr then abort;
      FlatAmt := amt;
      If DrCr = Cr then
        FlatAmt := FlatAmt * -1;
      _flatBal := _flatBal + FlatAmt;
    end;

  Function TDrCrCalculator.Balance:Integer;
    begin
      Result := Abs(_flatBal);
    end;

  Function TDrCrCalculator.DrCr:TDrCr;
    begin
      If _flatBal < 0 then Result := Cr else Result := Dr;
    end;

end.

