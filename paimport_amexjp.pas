unit paimport_amexjp;
// American Express Japan CSV Import

{$mode objfpc}{$H+}
{$INTERFACES CORBA }

interface

uses
 Classes, SysUtils,
 //CSVDocument, // CSVDocument is separate from PlusAlpha project.
 paCurrency, md5,
 paImport,
 paImportMap;

type
TAmexJPEntry=Record
  TransactionDate:TDate;
  LocalCurrencyAmount:Real;
  Memo:UTF8String;
  ForeignCurrencyAmount:Real;
  ForeignCurrencyCode:TCurrCode;
  MemoHash:TMD5Digest;
end;

TAmexJPCSVImport = class(TInterfacedObject, IImportInterface)
//  FDoc: TCSVDocument;
  RowCount:Integer;
  ColCount:Integer;
  _CSVImport:TCSVImport;
  _CurrentRow:Integer;
  _EOF:Boolean;
  _CurrentEntry:TAmexJPEntry;
  InterfaceGUID:TGUID;
  _TestMode:Boolean;
Private
  Procedure DecodeRow;
public
  Constructor Create();
  Destructor Destroy();
  Procedure CreateTransaction;
  Procedure SetFileName(Const FileName:UTF8String);
  Property Data:TAmexJPEntry read _CurrentEntry;
  Procedure GetNext;
  Property TestMode:Boolean read _TestMode write _TestMode;
  property EOF:Boolean Read _EOF;
//    Function GetValue(const aCol:Integer; aRow:Integer):UTF8String;
  Function GetRowCount:Integer;
  Function GetCurrentRowValue(Const ColNum:Integer):UTF8string;
end;



implementation

Uses
 paJournal, LibPa;

ResourceString
  MSGTRANSINSERTED = 'Transaction Inserted';
  ERRTRANSNOTINSERT = 'Error Inserting Transaction.';
  ERRTRANSNOTVALID = 'Transaction is not valid.';
  ERRTRANSNOTBAL =  'Transaction is not balanced.';
  MSGTRANSINSSUCC = 'Transaction Inserted.';
  MSGTRANSINSERR = 'Error Inserting Transaction!';


Constructor TAmexJPCSVImport.Create();
  begin
    InterfaceGUID := StringToGUID('{3CA61E92-916C-4A0E-B94A-E7F973B4DB79}');
    TestMode := True;
    _EOF := True;
    _CSVImport := TCSVImport.create();
  end;

Procedure TAmexJPCSVImport.SetFileName(Const FileName:UTF8String);
  begin
    _CurrentRow := -1;
    _EOF := False;
    _CSVImport.SetFileName(FileName);
  end;

Procedure TAmexJPCSVImport.GetNext;
  begin
    Inc(_CurrentRow);
    If (_CurrentRow = (_CSVImport.GetRowCount-1)) then _EOF := True;
    DecodeRow;
  end;

Function TAmexJPCSVImport.GetCurrentRowValue(Const ColNum:Integer):UTF8string;
  begin
    Result := _CSVImport.GetValue(ColNum, _CurrentRow)
  end;

Procedure TAmexJPCSVImport.DecodeRow;
  Const
    DateCol=0;
    LocalCurrencyAmtCol=1;
    MemoCol=2;
    ForeignCurrencyCol=3;
  Var
    tmpAmt:UTF8String;
    tmpFC:UTF8String;   // Raw Data with Two fields
    tmpFCAmt:UTF8String;
//    tmpRow:ANSIString;// Temporary row for constructing hash
  begin
    // Convert Date from string to TDate
    // Date should be converted from Japan to GMT, but since we don't know the time...
    _CurrentEntry.TransactionDate := StrToDate(GetCurrentRowValue(DateCol), '/');
    // Remove Quotation Marks, Commas if needed, convert to Integer
    tmpAmt := Strip_Comma(GetCurrentRowValue(LocalCurrencyAmtCol));
//    tmpAmt := StringReplace(tmpAmt,',','',[rfReplaceAll]);
    _CurrentEntry.LocalCurrencyAmount := StrToFloat(tmpAmt);
    // Remove Quotation Chars, Convert Encoding to UTF8 if required
    //    _CurrentEntry.Memo := ANSIToUTF8(_CSVImport.GetValue(MemoCol, _CurrentRow));
    _CurrentEntry.Memo := GetCurrentRowValue(MemoCol);
    With _CurrentEntry do MemoHash := MD5String(Memo);
    // Remove Quotation Marks
    // Split last field into Currency Amount and Currency Code
    tmpFC := GetCurrentRowValue(ForeignCurrencyCol);
    If tmpFC = '' then
      begin // No data, so we clear the fields
        _CurrentEntry.ForeignCurrencyAmount:= 0;
        _CurrentEntry.ForeignCurrencyCode:= '';
      end
    else
      begin
        // Extract the foreign currency code
        _CurrentEntry.ForeignCurrencyCode := Copy(tmpFC,1,3);
        // Remove foreign currency code so we can get at the actual numeric amount
        tmpFCAmt := StringReplace(tmpFC,_CurrentEntry.ForeignCurrencyCode,'',[rfReplaceAll]);
        // Remove Commas
//        tmpFCAmt := StringReplace(tmpFCAmt,',','',[rfReplaceAll]);
        tmpFCAmt := Strip_Comma(tmpFCAmt);
        _CurrentEntry.ForeignCurrencyAmount:= StrToFloat(tmpFCAmt);
      end;
    end; // of PROCEDURE

Procedure TAmexJPCSVImport.CreateTransaction;
// These constant values will be stored in the INTERFACELIST table
  CONST
    // Default Use Account GUIDs (Should be in config file or table)
    // American Express Japan Account (Liability) Default Cr
    GUID_AMEX_JAPAN='{4D1ABE22-2073-4779-A46B-A92DC6C46D23}';
    // Uncategorized Expenses Account (Expense)  Default Dr
    GUID_UNCAT_EXP ='{22EF5AE8-FD0D-49E5-945E-C88C8B5AA599}';
    // Payment Bank Account (Asset) / Payment Account
    GUID_PAYMENT_BANK='{80377524-79F5-40AC-A328-22DD4C73AF6A}';
    // Memo text used to detect a payment
    // This should really be an hash to save space and avoid mojibake.
    PAYMENT_MEMO_TEXT='前回分口座振替金額';

  var
    TN:Integer;
    NewEntry:Integer; // Line item number of new journal detail entries

  Procedure Process_Refund;
    begin
      With CompleteJournalEntry do
        begin
        NewEntry := AddDetailEntry;
        with _JournalDetailEntries[NewEntry] do
         begin
           TransNo := TN;
 //          AcctNo :=
           If ImportMapEntry.Load(self.InterfaceGUID, self.data.Memo) then
              AcctGuid := ImportMapEntry.AcctGUID   // Actual Mapped Account
            else
              AcctGuid := StringToGUID(GUID_UNCAT_EXP);// Uncategorized Expense
           TransRow := 0;
           Text := self.data.Memo;
           // This should really multiply the amount depending on the currency
           // f.e. Amount := AMT * 100 for USD
           Amount :=  Abs(FloatToDBAmount('JPY',  self.Data.LocalCurrencyAmount));
           DrCr:=Cr;
         end;
 // Credit Card
       NewEntry := AddDetailEntry;
       with _JournalDetailEntries[NewEntry] do
         begin
                     TransNo := TN;
                     AcctGuid := StringToGUID(GUID_AMEX_JAPAN); // Amex Japan
                     TransRow := 1;
                     Text := self.data.Memo;
                     // This should really multiply the amount depending on the currency
                     // f.e. Amount := AMT * 100 for USD
                     Amount := Abs(FloatToDBAmount('JPY',
                                               self.Data.LocalCurrencyAmount));
                     DrCr:=Dr;
         end;
        end;
 end;// [sub]PROCEDURE

  Procedure Process_Payment;
    begin
      //  Credit bank account
      With CompleteJournalEntry do
        begin
        NewEntry := AddDetailEntry;
        with _JournalDetailEntries[NewEntry] do
         begin
           TransNo := TN;
           AcctGuid := StringToGUID(GUID_PAYMENT_BANK);// Payment Bank
           TransRow := 0;
           Text := self.data.Memo;
           // This should really multiply the amount depending on the currency
           // f.e. Amount := AMT * 100 for USD
           Amount :=  Abs(FloatToDBAmount('JPY',  self.Data.LocalCurrencyAmount));
           DrCr:=Cr;
         end;
 //  Debit Card Liability Account
       NewEntry := AddDetailEntry;
       with _JournalDetailEntries[NewEntry] do
         begin
                     TransNo := TN;
                     AcctGuid := StringToGUID(GUID_AMEX_JAPAN); // Amex Japan
                     TransRow := 1;
                     Text := self.data.Memo;
                     // This Multiplies the amount depending on the currency
                     // f.e. Amount := AMT * 100 for USD
                     Amount := Abs(FloatToDBAmount('JPY',
                                               self.Data.LocalCurrencyAmount));
                     DrCr:=Dr;
         end;
        end;
    end;

  // Process an entry with a negative amount.
  // This is currently the same as a positive amount except that we make the
  // amounts positive and then swap the debit and credit to compensate.
  // (Note: This makes sense for reversals on the card, but not for card payments.)
  // Plan to add a cut-off value above which / Memo text where negative
  // transactions are treated as card payments from a default bank account.
  procedure Process_Negative;
    begin
      If self.data.Memo = PAYMENT_MEMO_TEXT Then
        Process_Payment
      Else
        Process_Refund;
    end;

  procedure Process_Positive;
    begin
  // Debit Expense
     With CompleteJournalEntry do
       begin
       NewEntry := AddDetailEntry;
       with _JournalDetailEntries[NewEntry] do
        begin
          TransNo := TN;
          If ImportMapEntry.Load(self.InterfaceGUID, self.data.Memo) then
            AcctGuid := ImportMapEntry.AcctGUID   // Actual Mapped Account
          else
            AcctGuid := StringToGUID(GUID_UNCAT_EXP);// Uncategorized Expense
          TransRow := 0;
          Text := self.data.Memo;
          // This multiplies the amount depending on the currency
          // f.e. Amount := AMT * 100 for USD
          Amount :=  FloatToDBAmount('JPY',  self.Data.LocalCurrencyAmount);
          DrCr:=Dr;
        end;
// Credit Card
      NewEntry := AddDetailEntry;
      with _JournalDetailEntries[NewEntry] do
        begin
                    TransNo := TN;
          //          AcctNo :=
                    AcctGuid := StringToGUID(GUID_AMEX_JAPAN); // Amex Japan
                    TransRow := 1;
                    Text := self.data.Memo;
                    // This multiplies the amount depending on the currency
                    // f.e. Amount := AMT * 100 for USD
                    Amount := FloatToDBAmount('JPY',
                                              self.Data.LocalCurrencyAmount);
                    DrCr:=Cr;
        end;
       end;
    end; // of [sub]PROCEDURE

  // Actual Method Body
  begin
    With CompleteJournalEntry do
    begin
     CompleteJournalEntry.Reset;
     TN := HighWaterMark + 1;
      With _JournalHeader do
        begin
          HdrTransNo := TN;
 //         CurrCode  := 'JPY'; // This is Amex JAPAN
          EffDate   := self.Data.TransactionDate;
          HdrMemo   := self.Data.Memo;
          HdrPosted := True;
        end;  // of with _JournalHeader
      // Add Exception here for negative amounts.
      if self.Data.LocalCurrencyAmount < 0 then
        Process_Negative
      else
        Process_Positive;

   If not IsBalanced then
     begin
       Raise Exception.Create(ERRTRANSNOTBAL);
       exit;
     end;

   If not Validate then
     begin
       Raise Exception.Create(ERRTRANSNOTVALID);
       exit;
     end;

     Case TestMode of
       False:begin
         If Insert then
           DebugLn(MSGTRANSINSSUCC) //'Transaction Inserted.')
         else
           Raise Exception.Create(MSGTRANSINSERR);//'Error Inserting Transaction!');
       end; // of TestMode False
       True:;//CompleteJournalEntry.Reset;
      end; // of CASE TestMode of

    end; // of with CompleteJournalEntry

  end; // of TAmexJPCSVImport.CreateTransaction;

Function TAmexJPCSVImport.GetRowCount:Integer;
  begin
    Result := _CSVImport.RowCount;
  end;

Destructor TAmexJPCSVImport.Destroy();
  begin
    _CSVImport.free();
    inherited;
  end;


end.

