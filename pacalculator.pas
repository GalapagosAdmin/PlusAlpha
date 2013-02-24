unit paCalculator;

{$mode objfpc}{$H+}

interface

 uses
  Classes, SysUtils, libpa;

 type
   TDrCrCalculator  = class (Tobject)
     private
       _flatBal:Integer; // flat single number balance
                         // +:dr, -:Cr
     public
       Procedure clear;
       Procedure AddEntry(amt:integer; DrCr:TDrCr);
       Function Balance:Integer;
       Function DrCr:TDrCr;
   end;



implementation

  procedure TDrCrCalculator.clear;
    begin
      _flatBal := 0;
    end;

  Procedure TDrCrCalculator.AddEntry(amt:integer; DrCr:TDrCr);
    var
      FlatAmt:Integer;
    begin
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

