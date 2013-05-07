Unit paLedger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, libpa, paCurrency;//, comctrls;


  type
  TLedgerAccount = Class(TObject)   // Ledger Account
  protected
      _bal:LongInt; // Ledger balance
      _scale:Integer; // Decimals for currency scale
      _drcr:Tdrcr; // debit/credit indicator for current balance
      _dirty:boolean; // Indicates if synch to db is required
      _new:boolean;    // specifies that this entry has not yet been written to the database
      _AccTypeDB:Char; // Account Type Code for the database (Char1)
      _AccStCdDB:Char; // Account sub-type
  private
      _AcctNo:TInteger; // Internal Account No.
      _Text:TUTF8String; // Account Description
      _AcctType:TAcctType; // Account Type (Pascal Enumerated)
      _currency:TCurrCode;  // Currency Code
      _AccStDB:Char; // Account Subtype Code for the database (Char1)
      _TextKey:TInteger; // Text key used for i18n of text
      _TransNo:TInteger; // Latest Transaction number posted to this ledger account
      _ExtRefNo:TInteger; // External account number (i.e. account number at the bank)
      _Guid:TGuid;  // Globally Unique Identifier for this account.  Used for DB merges.
      Procedure SetBal(NewBalance:LongInt);
      Procedure SetDrCr(NewDrCr:TDrCr);
      Procedure SetTransNo(NewTransNo:TInteger);
      Procedure SetText(NewText:TUTF8String);
      Procedure SetCurrency(NewCurrency:TCurrCode);
      Procedure SetAcctType(NewAcctType:TAcctType);
      Procedure SetAcctNo(NewAcctNo:Integer);
      Function Insert:Boolean;
      Function Update:Boolean;
      Function _DrBal:LongInt;   // converts to the db format for now
      Function _CrBal:LongInt;   // converts to the db format for now
    Public
      Constructor create;
      Property AcctNo:TInteger read _AcctNo write SetAcctNo;
      Property Text:TUTF8String read _Text write SetText;
      Property Balance:LongInt read _bal write SetBal;
      Property Currency:TCurrCode read _Currency write SetCurrency;
      Property DrCr:Tdrcr read _drcr write SetDrCr;
      Property AccountType:TAcctType read _AcctType write SetAcctType;
      Property AccountSubType:Char read _AccSTDB;
      Property TransNo:TInteger read _TransNo write SetTransNo;
      Function Load(AccountNo:TInteger):boolean;
      Property ExtAcctNo:TInteger read _ExtRefNo;
      Function Synch:boolean;
      Procedure Commit;
  end;


  // Entire general ledger
  TAccountList = Class(TObject)
    private
      _Loaded:Boolean;
      _AccountList : Array of TLedgerAccount;
      _CurrentAccount:integer; // used for enumerating
      _HighWaterMark:Integer;  // Highest numbered account in the database
   //   Tree:TTreeNodes;
      Procedure UpdateHighWaterMark;
      Procedure Load; // Loads the account list / Accounts from the DB (All)
      // Loads the account list / Accounts from the DB, after filtering by name.
      Procedure LoadNameFilter(const AccountName:UTF8String);
      Procedure Clear;
    public
     Constructor Create; //overload;
     Constructor Create(const AccountName:UTF8String); //overload;
     Procedure ReLoad;
     // loads an existing ledger account from the database into a new object
     Procedure AddAccount(const AcctNo:Integer);
     Function AccountStringList:TStringList;
     Function GetFirstAccount:TLedgerAccount;
     Function GetNextAccount:TLedgerAccount;
     Function GetAccountNo(AccountNo:TInteger):TLedgerAccount;
     Function EOF:Boolean;
     Function GetNextFreeAccountNo:Integer;
  end;




  var
      AccountList:TAccountList;


implementation

uses
  //db,
  sqldb, paDatabase, LazLogger;

Constructor TLedgerAccount.Create;
  begin
    _dirty := true;
    _new := true;
    _bal := 0;
    _AccStCdDB := 'X';
    _TransNo := -1;
  end;

Function TLedgerAccount._DrBal:LongInt;
  begin
    Case DrCr of
      Dr: Result := _bal;
      Cr: Result := 0;
    end;
  end;

Function TLedgerAccount._CrBal:LongInt;
   begin
     Case DrCr of
       Cr: Result := _bal;
       Dr: Result := 0;
     end;
   end;

Function TLedgerAccount.Load(AccountNo:Integer):Boolean;
  var
  //  i:TInteger;
    SQLQuery1:TSQLQuery;
    DrBal, CrBal:TInteger;
    rows:integer;
    test:boolean;
  begin
//  DebugLn(IntToStr(AccountNo));
  SQLQuery1 := TSQLQuery.Create(nil);
  SQLQuery1.Transaction := SQLTransaction1;
  SQLQuery1.SQL.Text := 'select AcctNo, DrBal, CrBal, CurrKey, Text, AccTypeCd, AccSTCd '
  + ' from ledger where AcctNo = :acctno';
//   + ' from ledger where rowid = :rowid';
  test := assigned(SQLTransaction1);
  SqlQuery1.ParamByName('acctno').AsInteger := AccountNo;

  SQLQuery1.Open;
  rows := SQLQuery1.RowsAffected;
// The following doesn't work for SELECT statements
//  Result := Rows = 1;
  // Not working for some reason, rows always = 0, EOF = True.
//   While not SQLQuery1.EOF do
  Result := Not SQLQuery1.EOF;
  If not SQLQuery1.EOF then
   begin
     With SQLQuery1 do
       begin
         try
           self._AcctNo := StrToInt(FieldByName('AcctNo').AsString);
         except
          DebugLn('TLedgerAccount.Load:Error while reading acctno'); 
         end;
         try
          self._currency := FieldByName('CurrKey').AsString;
          self._scale := GetScale(_Currency);
         except
          DebugLn('TLedgerAccount.Load:Error while reading currency'); 
         end;
         try
          self._AccTypeDB := FieldByName('AccTypeCd').AsString[1];
          self._AcctType := TAcctType(StrtoInt(abap_translate(self._AccTypeDB, AcctTransMap)));
         except
          DebugLn('TLedgerAccount.Load:Error while reading AcctType'); 
         // Pascal representation
         end;
         try
           self._AccStDB := FieldByName('AccSTCd').AsString[1];
         except
          DebugLn('TLedgerAccount.Load:Error while reading AcctSt field, record='+IntToStr(AccountNo)); 
         end;
         try
          self._Text := FieldByName('Text').AsString;
         except
          DebugLn('TLedgerAccount.Load:Error while reading text field'); 
         end;
         try
          DrBal :=  FieldByName('DrBal').AsLongInt;
          CrBal :=  FieldByName('CrBal').AsLongInt;
         except
          DebugLn('TLedgerAccount.Load:Error while reading DrBal/CrBal'); 
         end;
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
     // We just loaded the data, so it's clean.
    _Dirty := False;
    _New := False;
  end;

Procedure TLedgerAccount.SetText(NewText:TUTF8String);
  begin
    _Text := NewText;
    // We need up update the DB now, so set _dirty to true;
    _Dirty := True;
  end;

Procedure TLedgerAccount.SetBal(NewBalance:LongInt);
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

Procedure TLedgerAccount.SetCurrency(NewCurrency:TCurrCode);
  begin
    _Currency := NewCurrency;
    // We need up update the DB now, so set _dirty to true;
    _Dirty := True;
  end;

Procedure TLedgerAccount.SetAcctType(NewAcctType:TAcctType);
 var
//  c:Char;
  tmpStr:String;
  begin
    _AcctType := NewAcctType;
    // update the DB account type
//    c := IntToStr(Ord(Self._AcctType));
      tmpStr :=    ABAP_Translate(IntToStr(Ord(AccountType)), AcctTransMapRev);   
      _AccTypeDB := tmpStr[1]; 

	   // We need up update the DB now, so set _dirty to true;
    _Dirty := True;
  end;

Procedure TLedgerAccount.SetAcctNo(NewAcctNo:Integer);
  begin
    _AcctNo := NewAcctNo;
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
        + ' DrBal2, CrBal2, "AccStCd" ) '
        + 'values ( :AcctNo, :DrBal, :CrBal, '
        +         ' :CurrKey, :Text, :AccTypeCd, '
        +         ' :PAcctNo, :TransNo, :PayeeCd, '
        +         ' :DrBal2, :CrBal2, :AccStCd)';

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
    SqlQuery1.ParamByName('AccStCd').AsString := self._AccStCdDB;

//    SqlQuery1.ParamByName('updatedate').AsString := DateTimetoYYYYMMDD(now);
//    SqlQuery1.ParamByName('effdate').AsString := DateTimetoYYYYMMDD(_EffDate);

    SQLQuery1.ExecSQL;
    SQLQuery1.Close;
    Result := (SQLQuery1.RowsAffected = 1);
    SQLQuery1.Destroy;

 end;

Procedure TLedgerAccount.commit;
  begin
    SQLTransaction1.Commit;
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

Function TAccountList.GetFirstAccount:TLedgerAccount;
  begin
    if self.eof then exit;
     _CurrentAccount := Low(_AccountList);
     Result := _AccountList[_CurrentAccount];
  end;

Function TAccountList.GetNextAccount:TLedgerAccount;
  begin
    if self.eof then exit;
    Inc(_CurrentAccount);
    Result := _AccountList[_CurrentAccount];
  end;

Function TAccountLIst.EOF:Boolean;
  begin
     Result := (_CurrentAccount = High(_AccountList));
     // Catch case of zero results
     If Length(_AccountList) = 0 then Result := True;
  end;

// clears the accounts without saving any changes to the database
Procedure TAccountList.Clear;
  var
    ThisAccount:TLedgerAccount;
  begin
    for ThisAccount in _AccountList do
        ThisAccount.Free;
    SetLength(_AccountList, 0);
  end;

Procedure TAccountList.ReLoad;
  begin
    // Clearly this is not the most efficient way...
    Clear;
    Load;
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

Procedure TAccountList.Load; // Loads the account listing from the database
  var
//     FDataset: TSdfDataset;
    i:integer;
    SQLQuery1:TSQLQuery;
    TmpStr : TUTF8String;
//    tmpAccount:TLedgerAccount;
  begin
//    Tree := TTreeNodes.Create(nil); // create account tree
//  for i := 0 to 24 do

  // Update the account list.
  // This should really be moved to a separate refresh procedure to allow
  // updates after loading.
  try
    if _Loaded then Clear;
    SQLQuery1 := TSQLQuery.Create(nil);
    SQLQuery1.Transaction := SQLTransaction1;
    SQLQuery1.SQL.Text := 'select AcctNo from ledger';
    SQLQuery1.open;
    _Loaded := True;
    While not SQLQuery1.EOF do
     begin
       With SQLQuery1 do
         AddAccount(FieldByName('AcctNo').AsInteger);
       SQLQuery1.Next;
     end;
  finally
    SQLQuery1.Close;
    SQLQuery1.Destroy;
  end; // of TRY..FINALLY
// update the tree
//   tmpAccount := GetAccountNo(0);  // 0 is the root, by definition
//   Tree.AddObject(nil, tmpAccount.Text, tmpAccount);
  end;

Procedure TAccountList.LoadNameFilter(Const AccountName:UTF8String);
// Loads the account listing from the database, filtering by name.
  var
//     FDataset: TSdfDataset;
    i:integer;
    SQLQuery1:TSQLQuery;
    TmpStr : TUTF8String;
//    tmpAccount:TLedgerAccount;
  begin
   if _Loaded then Clear;
//    Tree := TTreeNodes.Create(nil); // create account tree
//  for i := 0 to 24 do

  // Update the account list.
  // This should really be moved to a separate refresh procedure to allow
  // updates after loading.
  try
    SQLQuery1 := TSQLQuery.Create(nil);
    SQLQuery1.Transaction := SQLTransaction1;
{ TODO 2 -oshiruba -cHardening : Convert LIKE to parameter, if possible }
{ TODO 2 -oshiruba -cI18N : Convert LIKE to parameter to use text table + language }
    SQLQuery1.SQL.Text := 'select AcctNo from ledger where text like ''%' + AccountName + '%''';
    Writeln(SQLQuery1.SQL.Text);
    SQLQuery1.open;
    _Loaded := True;
    While not SQLQuery1.EOF do
     begin
       With SQLQuery1 do
         AddAccount(FieldByName('AcctNo').AsInteger);
       SQLQuery1.Next;
     end;
  finally
    SQLQuery1.Close;
    SQLQuery1.Destroy;
  end; // of TRY..FINALLY
// update the tree
//   tmpAccount := GetAccountNo(0);  // 0 is the root, by definition
//   Tree.AddObject(nil, tmpAccount.Text, tmpAccount);
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

  SQLQuery1.SQL.Text := 'select max(acctno) as hwm from ledger';
  SQLQuery1.open;
  If not SQLQuery1.EOF then
    _HighWaterMark := SqlQuery1.FieldByName('hwm').AsInteger;
  SQLQuery1.Close;
  SQLQuery1.Destroy;

  end;

Function TAccountList.GetNextFreeAccountNo:Integer;
  begin
    // Get the latest high water mark from the database
    UpdateHighWaterMark;
    Result := _HighWaterMark + 1;
    // This is in case we get called multiple times by different clients
    Inc(_HighWaterMark);
  end;


// Regular Constructor - Load All Accounts
Constructor TAccountList.Create();
  begin
    _Loaded := False;
    Load;
  end;

// Modified Constructor - Load only accounts Matching Account Name Pattern
Constructor TAccountList.Create(const AccountName:UTF8String); //overload;
  begin
    _Loaded := False;
    LoadNameFilter(AccountName);
  end;


initialization
  AccountList := TAccountList.Create;



end.

