unit paTransactionList;
// Generates a list of transactions
// Example: Used for listing all of the transactions involving a certain account
// for a certain period of time.
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type
  TResultEntry=Record
    TransactionNo:Integer;
    TransactionRow:Integer;
  end;

  TResultArray=Array of TResultEntry;
  TTransactionList=Class(TObject)
  Private
    _SearchAccount:Integer; // Account of Interest
    _Array:TResultArray;
    Procedure Select;
    Procedure SetAccount(const NewAccount:Integer);
  Public
    Property Account:Integer read _SearchAccount write SetAccount;
    Property TransNos:TResultArray read _Array;
  end;

 var
    TransactionList : TTransactionList;

implementation

Uses
   sqldb, paDatabase;

Procedure TTransactionList.Select;
var
   SQLQuery1:TSQLQuery;
  begin
    SetLength(_Array, 0);
    SQLQuery1 := TSQLQuery.Create(nil);

  //Journal Header should always be inserted first, so it's safer to take that
  // number
  With SQLQuery1 do
    begin
      Transaction := SQLTransaction1;
      SQL.Text := 'SELECT TRANSNO, TRANSROW FROM JOURNAL WHERE DRACCTCD = :AcctNo ';;
      ParamByName('AcctNo').AsInteger := _SearchAccount;

      Open;
      While not EOF do
        begin
           SetLength(_Array, Length(_Array)+1);
           _Array[High(_Array)].TransactionNo := FieldByName('TRANSNO').AsInteger;
           _Array[High(_Array)].TransactionRow := FieldByName('TRANSROW').AsInteger;
           next;
        end;
      Close;
      Destroy;
    end;
  end;

Procedure TTransactionList.SetAccount(const NewAccount:Integer);
  begin
    _SearchAccount := NewAccount;
    Select;
  end;

initialization
 TransactionList := TTransactionList.Create;

end.

