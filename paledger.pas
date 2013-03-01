unit paLedger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, libpa;


  type
  TLedgerAccount = Class(TObject)   // Ledger Account
  protected
   _bal:Integer; // Ledger balance
   _drcr:Tdrcr; // debit/credit indicator for current balance
   _dirty:boolean; // Indicates if synch to db is required
   _new:boolean;    // specifies that this entry has not yet been written to the database
  private
      _AcctNo:TInteger; // Internal Account No.
      _Text:TUTF8String; // Account Description
      _AcctType:TAcctType;
      _currency:TCurrCode;  // Currency Code
      _AccTypeDB:Char; // Account Type Code for the database
      _AccStDB:Char; // Account Subtype Code for the database
      _TextKey:TInteger; // Text key used for i18n of text
      _TransNo:TInteger; // Latest Transaction number posted to this ledger account
      _ExtRefNo:TInteger; // External account number (i.e. account number at the bank)
      Procedure SetBal(NewBalance:TInteger);
      Procedure SetDrCr(NewDrCr:TDrCr);
      Procedure SetTransNo(NewTransNo:TInteger);
      Function Insert:Boolean;
      Function Update:Boolean;
      Function _DrBal:Integer;   // converts to the db format for now
      Function _CrBal:Integer;   // converts to the db format for now
    public
      constructor create;
      Property AcctNo:TInteger read _AcctNo;
      Property Text:TUTF8String read _Text;
      Property Balance:TInteger read _bal write SetBal;
      Property Currency:TCurrCode read _Currency;
      Property DrCr:Tdrcr read _drcr write SetDrCr;
      Property AccountType:Char read _AccTypeDB;
      Property AccountSubType:Char read _AccSTDB;
      Property TransNo:TInteger read _TransNo write SetTransNo;
      Function Load(AccountNo:TInteger):boolean;
      Function Synch:boolean;
  end;


  // Entire general ledger
  TAccountList = Class(TObject)
    private
      _AccountList : Array of TLedgerAccount;
      _CurrentAccount:integer; // used for enumerating
      _HighWaterMark:Integer;  // Highest numbered account in the database
      Procedure UpdateHighWaterMark;
    public
     Constructor Create; //overload;
     // loads an existing ledger account from the database into a new object
     Procedure AddAccount(const AcctNo:Integer);
     Function AccountStringList:TStringList;
     Function GetFirstAccount:TLedgerAccount;
     Function GetNextAccount:TLedgerAccount;
     Function GetAccountNo(AccountNo:TInteger):TLedgerAccount;

     Function EOF:Boolean;
  end;




  var
      AccountList:TAccountList;


implementation

uses
  //db,
  sqldb, paDatabase;

Constructor TLedgerAccount.Create;
  begin
    _dirty := true;
    _new := true;
    _bal := 0;
    _TransNo := -1;
  end;

Function TLedgerAccount._DrBal:Integer;
  begin
    Case DrCr of
      Dr: Result := _bal;
      Cr: Result := 0;
    end;
  end;

Function TLedgerAccount._CrBal:Integer;
   begin
     Case DrCr of
       Cr: Result := _bal;
       Dr: Result := 0;
     end;
   end;


Function TLedgerAccount.Load(AccountNo:Integer):Boolean;
  var
    i:TInteger;
    SQLQuery1:TSQLQuery;
    DrBal, CrBal:TInteger;
    rows:integer;
    test:boolean;
  begin
  SQLQuery1 := TSQLQuery.Create(nil);
  SQLQuery1.Transaction := SQLTransaction1;
  SQLQuery1.SQL.Text := 'select AcctNo, DrBal, CrBal, CurrKey, Text, AccTypeCd '
  + ' from ledger where AcctNo = :acctno';
//   + ' from ledger where rowid = :rowid';
  test := assigned(SQLTransaction1);
  SqlQuery1.ParamByName('acctno').AsInteger := AccountNo;

  SQLQuery1.Open;
  rows := SQLQuery1.RowsAffected;

  // Not working for some reason, rows always = 0, EOF = True.
//   While not SQLQuery1.EOF do
  If not SQLQuery1.EOF then
   begin
     With SQLQuery1 do
       begin
         self._AcctNo := StrToInt(FieldByName('AcctNo').AsString);
         self._currency := FieldByName('CurrKey').AsString;
         self._AccTypeDB := FieldByName('AccTypeCd').AsString[1];
         self._Text := FieldByName('Text').AsString;
         DrBal :=  FieldByName('DrBal').AsInteger;
         CrBal :=  FieldByName('CrBal').AsInteger;
//     SQLQuery1.Next;

       end; // of WITH
   end; // of IF
  SQLQuery1.Close;
  SQLQuery1.Destroy;

    If DrBal > 0 then
      begin
       self._bal := DrBal;
       self._DRCR := Dr;
      end
    else
     begin
       self._bal := CrBal;
       self._DRCR := Cr;
     end;
     // We just loaded the data, do it's clean.
    _Dirty := False;
    _New := False;
  end;

Procedure TLedgerAccount.SetBal(NewBalance:Integer);
  begin
    _bal := NewBalance;
    // We need up update the DB now, so set _dirty to true;
    _Dirty := True;
  end;

Procedure TLedgerAccount.SetDrCr(NewDrCr:TDrCr);
  begin
    _DrCr := NewDrCr;
    // We need up update the DB now, so set _dirty to true;
    _Dirty := True;
  end;


Procedure TLedgerAccount.SetTransNo(NewTransNo:TInteger);
  begin
    _TransNo := NewTransNo;
    // We need up update the DB now, so set _dirty to true;
    _Dirty := True;
  end;

Function TLedgerAccount.Insert:boolean;
  var
    SQLQuery1:TSQLQuery;
    TmpStr : TUTF8String;
  begin
    // validations
    If self._TransNo = 0 then exit(false);
    if self._Text = '' then exit(False);

    SQLQuery1 := TSQLQuery.Create(nil);
    SQLQuery1.Transaction := SQLTransaction1;
    SQLQuery1.SQL.Text := 'insert into "main"."Ledger" ('
        + '"AcctNo", "DrBal", "CrBal", '
        + ' "CurrKey", "Text", "AccTypeCd", '
        + ' PACCTNO, TRANSNO, PayeeCd, '
        + ' DrBal2, CrBal2 ) '
        + 'values ( :AcctNo, :DrBal, :CrBal, '
        +         ' :CurrKey, :Text, :AccTypeCd, '
        +         ' :PAcctNo, :TransNo, :PayeeCd, '
        +         ' :DrBal2, :CrBal2 )';

    SqlQuery1.ParamByName('AcctNo').AsInteger := self._AcctNo;
    SqlQuery1.ParamByName('DrBal').AsInteger := self._DrBal;
    SqlQuery1.ParamByName('CrBal').AsInteger := self._CrBal;

    SqlQuery1.ParamByName('CurrKey').AsString := self._currency;
    SqlQuery1.ParamByName('Text').AsString := self._Text;
    SqlQuery1.ParamByName('AccTypeCd').AsString := self._AccTypeDB;

    SqlQuery1.ParamByName('PAcctNo').AsInteger := -1;
    SqlQuery1.ParamByName('TransNo').AsInteger := _TransNo;
    SqlQuery1.ParamByName('PayeeCd').AsInteger := -1;

    SqlQuery1.ParamByName('DrBal2').AsInteger := self._DrBal;
    SqlQuery1.ParamByName('CrBal2').AsInteger := self._CrBal;

//    SqlQuery1.ParamByName('updatedate').AsString := DateTimetoYYYYMMDD(now);
//    SqlQuery1.ParamByName('effdate').AsString := DateTimetoYYYYMMDD(_EffDate);

    SQLQuery1.ExecSQL;
    SQLQuery1.Close;
    Result := (SQLQuery1.RowsAffected = 1);
    SQLQuery1.Destroy;

 end;

// Update existing account
Function TLedgerAccount.update:boolean;
  var
    SQLQuery1:TSQLQuery;
    TmpStr : TUTF8String;
  begin
    // validations
    If self._TransNo = 0 then exit(false);
    if self._Text = '' then exit(False);

    SQLQuery1 := TSQLQuery.Create(nil);
    SQLQuery1.Transaction := SQLTransaction1;
    SQLQuery1.SQL.Text := 'Update "main"."Ledger" '
        + ' SET DrBal = :DrBal, '
        + '     CrBal = :CrBal, '
        + '     CurrKey = :CurrKey, '

        + '     Text = :Text, '
        + '     AccTypeCd = :AccTypeCd, '
        + '     PACCTNO = :PAcctNo, '

        + '     TRANSNO = :TransNo, '
        + '     PayeeCd = :PayeeCd, '
        + '     DrBal2 = :DrBal2, '
        + '     CrBal2 = :CrBal2 '
        + ' Where AcctNo = :AcctNo';

    SqlQuery1.ParamByName('DrBal').AsInteger := self._DrBal;
    SqlQuery1.ParamByName('CrBal').AsInteger := self._CrBal;
    SqlQuery1.ParamByName('CurrKey').AsString := self._currency;

    SqlQuery1.ParamByName('Text').AsString := self._Text;
    SqlQuery1.ParamByName('AccTypeCd').AsString := self._AccTypeDB;
    SqlQuery1.ParamByName('PAcctNo').AsInteger := -1;

    SqlQuery1.ParamByName('TransNo').AsInteger := _TransNo;
    SqlQuery1.ParamByName('PayeeCd').AsInteger := -1;
    SqlQuery1.ParamByName('DrBal2').AsInteger := self._DrBal;
    SqlQuery1.ParamByName('CrBal2').AsInteger := self._CrBal;

    SqlQuery1.ParamByName('AcctNo').AsInteger := self._AcctNo;


//    SqlQuery1.ParamByName('updatedate').AsString := DateTimetoYYYYMMDD(now);
//    SqlQuery1.ParamByName('effdate').AsString := DateTimetoYYYYMMDD(_EffDate);

    SQLQuery1.ExecSQL;
    SQLQuery1.Close;
    Result := (SQLQuery1.RowsAffected = 1);
    SQLQuery1.Destroy;

 end;



Function TLedgerAccount.Synch:Boolean;
 begin
   if not _dirty then
     begin
        Result := true;
        exit;
     end;
   if _new then result := self.insert else result := self.update;
 end;


// Adds an account existing in the database to the in-memory list
Procedure TAccountList.AddAccount(const AcctNo:Integer);
  var
    TmpAcct:TLedgerAccount;
  begin
    TmpAcct := TLedgerAccount.Create;
    // Add entry to the in-memory array
    SetLength(_AccountLIst, Length(_AccountList)+1);
    // Changed to delegate loading to the Ledger class itself
    TmpAcct.Load(AcctNo);
    _AccountList[High(_AccountList)] := TmpAcct;
  end;

Function TAccountLIst.GetFirstAccount:TLedgerAccount;
  begin
     _CurrentAccount := Low(_AccountList);
     Result := _AccountList[_CurrentAccount];
  end;

Function TAccountLIst.GetNextAccount:TLedgerAccount;
  begin
     Inc(_CurrentAccount);
     Result := _AccountList[_CurrentAccount];
  end;

Function TAccountLIst.EOF:Boolean;
  begin
     Result := (_CurrentAccount = High(_AccountList));
  end;

// Gets the account you want from the array based on account number
Function TAccountLIst.GetAccountNo(AccountNo:TInteger):TLedgerAccount;
  var
//    i:TInteger;
    ThisAccount:TLedgerAccount;
  begin
    Result := nil;
    // Not very efficient, but then we shouldn't really have more than 100 or
    // so accounts
//    for i := Low(_AccountList) to high(_AccountList) do
      for ThisAccount in _AccountLIst do
        if ThisAccount.AcctNo = AccountNo then
          begin
            Result := ThisAccount;
            exit;
          end;

  end;

Constructor TAccountList.Create();
  var
//     FDataset: TSdfDataset;
    i:integer;
    SQLQuery1:TSQLQuery;
//     SQLTransaction1:TSQLTransaction;
//     SQLite3Connection1:TSQLite3Connection;
    TmpStr : TUTF8String;
  begin

//  for i := 0 to 24 do

  // Update the account list.
  // This should really be moved to a separate refresh procedure to allow
  // updates after loading.
  SQLQuery1 := TSQLQuery.Create(nil);
  SQLQuery1.Transaction := SQLTransaction1;
  SQLQuery1.SQL.Text := 'select AcctNo from ledger';
  SQLQuery1.open;
  While not SQLQuery1.EOF do
   begin
     With SQLQuery1 do
       AddAccount(FieldByName('AcctNo').AsInteger);
     SQLQuery1.Next;
   end;
  SQLQuery1.Close;
  SQLQuery1.Destroy;

  end;

Function TAccountList.AccountStringList:TStringList;
  var
    i:Integer;
    Acct:TLedgerAccount;
  begin
    Result := TStringList.Create;
 //   for i := low(_AccountList) to high(_AccountList) do
    for Acct in _AccountList do
      //     Result.Append(IntToStr(_AccountList[i]._AcctNo) + ' - ' + _AccountList[i]._Text);
       Result.Append(IntToStr(Acct._AcctNo) + ' - ' + Acct._Text);
  end;

Procedure TAccountList.UpdateHighWaterMark;
  var
   SQLQuery1:TSQLQuery;
  begin
    SQLQuery1 := TSQLQuery.Create(nil);
    SQLQuery1.Transaction := SQLTransaction1;

  //Journal Header should always be inserted first, so it's safer to take that
  // number
  SQLQuery1.SQL.Text := 'select max(acctno) as hwm from ledger';
  SQLQuery1.open;
  If not SQLQuery1.EOF then
    _HighWaterMark := SqlQuery1.FieldByName('hwm').AsInteger;
  SQLQuery1.Close;
  SQLQuery1.Destroy;

  end;


initialization
  AccountList := TAccountList.Create;



end.

