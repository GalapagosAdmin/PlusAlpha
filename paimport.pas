unit paImport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  CSVDocument, // CSVDocument is separate from PlusAlpha project.
  paCurrency, md5,
  paImportMap;

ResourceString
  ERRNOSUCHROW = 'Invalid Row Reference reading CSV file.';


type
  TCSVImport = class(TObject)
    FDoc: TCSVDocument;
    RowCount:Integer;
    ColCount:Integer;
   public
    Constructor Create();
    Destructor Destroy();
    Procedure SetFileName(Const FileName:UTF8String);
    Function GetValue(const aCol:Integer; aRow:Integer):UTF8String;
    Function GetRowCount:Integer;
    Function GetRowMD5Hash(Const Row:Integer):TMD5Digest;
  end;

  TAmexJPEntry=Record
    TransactionDate:TDate;
    LocalCurrencyAmount:Real;
    Memo:UTF8String;
    ForeignCurrencyAmount:Real;
    ForeignCurrencyCode:TCurrCode;
    MemoHash:TMD5Digest;
  end;

  TAmexJPCSVImport = class(TObject)
    FDoc: TCSVDocument;
    RowCount:Integer;
    ColCount:Integer;
    _CSVImport:TCSVImport;
    _CurrentRow:Integer;
    _EOF:Boolean;
    _CurrentEntry:TAmexJPEntry;
    InterfaceGUID:TGUID;
  Private
    Procedure DecodeRow;
  public
    Constructor Create();
    Destructor Destroy();
    Procedure CreateTransaction;
    Procedure SetFileName(Const FileName:UTF8String);
    Property Data:TAmexJPEntry read _CurrentEntry;
    Procedure GetNext;
    property EOF:Boolean Read _EOF;
//    Function GetValue(const aCol:Integer; aRow:Integer):UTF8String;
//    Function GetRowCount:Integer;
  end;


var
  CSVImporter:TCSVImport;


Implementation

Uses
 paJournal, LibPa;


ResourceString
  MSGTRANSINSERTED = 'Transaction Inserted';
  ERRTRANSNOTINSERT = 'Error Inserting Transaction.';
  ERRTRANSNOTVALID = 'Transaction is not valid.';
  ERRTRANSNOTBAL =  'Transaction is not balanced.';
  MSGTRANSINSSUCC = 'Transaction Inserted.';
  MSGTRANSINSERR = 'Error Inserting Transaction!';

// Generic CSV File Import Routine

Procedure TCSVImport.SetFileName(Const FileName:UTF8String);
  begin

    FDoc.LoadFromFile(FileName);
    RowCount := FDoc.RowCount;
    ColCount := FDoc.MaxColCount;
//      FDoc.CSVText := FileName;


  end;

Function TCSVImport.GetRowCount:Integer;
  begin
    Result := RowCount;
  end;

Function TCSVImport.GetValue(const aCol:Integer; aRow:Integer):UTF8String;
  begin
    Result := FDoc.Cells[aCol, aRow];  // col and row are zero based
  end;

// This should perhaps be a class helper for the FDoc
Function TCSVImport.GetRowMD5Hash(Const Row:Integer):TMD5Digest;
  Var
    Col:Integer;
    // We don't want to convert this to UTF8 since our intention here is only to
    // determine if it's the same row in the file as last time.
    tmpStr:ANSIString;
  begin
    If not FDoc.HasRow(Row) then
      Raise Exception.Create(ERRNOSUCHROW);
    tmpStr := '';
    For Col := 0 to (FDoc.ColCount[Row] - 1) do
      tmpStr := tmpStr + FDoc.Cells[Col, Row];
    Result := MD5String(tmpStr);
  end;

Constructor TCSVImport.Create();
  begin
    FDoc := TCSVDocument.Create;
    FDoc.Delimiter := ',';
    RowCount := 0;
    ColCount := 0;
  end;

Destructor TCSVImport.Destroy();
  begin
    inherited;
    FDoc.free;
  end;

// American Express Japan CSV Import

Constructor TAmexJPCSVImport.Create();
  begin
    InterfaceGUID := StringToGUID('{3CA61E92-916C-4A0E-B94A-E7F973B4DB79}');

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
    tmpRow:ANSIString;// Temporary row for constructing hash
  begin
    // Convert Date from string to TDate
    // Date should be converted from Japan to GMT, but since we don't know the time...
    _CurrentEntry.TransactionDate:= StrToDate(_CSVImport.GetValue(DateCol,_CurrentRow));
    // Remove Quotation Marks, Commas if needed, convert to Integer
    tmpAmt := _CSVImport.GetValue(LocalCurrencyAmtCol, _CurrentRow);
    tmpAmt := StringReplace(tmpAmt,',','',[rfReplaceAll]);
    _CurrentEntry.LocalCurrencyAmount := StrToFloat(tmpAmt);
    // Remove Quotation Chars, Convert Encoding to UTF8 if required
    _CurrentEntry.Memo := ANSIToUTF8(_CSVImport.GetValue(MemoCol, _CurrentRow));
    With _CurrentEntry do MemoHash := MD5String(Memo);
    // Remove Quotation Marks
    // Split last field into Currency Amount and Currency Code
    tmpFC := _CSVImport.GetValue(ForeignCurrencyCol, _CurrentRow);
    If tmpFC = '' then
      begin // No data, so we clear the fields
        _CurrentEntry.ForeignCurrencyAmount:= 0;
        _CurrentEntry.ForeignCurrencyCode:= '';
      end
    else
      begin
        _CurrentEntry.ForeignCurrencyCode:= Copy(tmpFC,1,3);
        tmpFCAmt := StringReplace(tmpFC,_CurrentEntry.ForeignCurrencyCode,'',[rfReplaceAll]);
        tmpFCAmt := StringReplace(tmpFCAmt,',','',[rfReplaceAll]);
        _CurrentEntry.ForeignCurrencyAmount:= StrToFloat(tmpFCAmt);

      end;
    end; // of PROCEDURE

Procedure TAmexJPCSVImport.CreateTransaction;
  CONST
    // Default Use Account GUIDs (Should be in config file or table)
    // American Express Japan Account (Liability)
    GUID_AMEX_JAPAN='{4D1ABE22-2073-4779-A46B-A92DC6C46D23}';
    // Uncategorized Expenses Account (Expense)
    GUID_UNCAT_EXP ='{22EF5AE8-FD0D-49E5-945E-C88C8B5AA599}';
    // Payment Bank Account (Asset)
    GUID_PAYMENT_BANK='{80377524-79F5-40AC-A328-22DD4C73AF6A}';
    // Memo text used to detect a payment
    // This should really be an hash to save space and avoid mojibake.
    PAYMENT_MEMO_TEXT='前回分口座振替金額';

  var
    TN:Integer;

  Procedure Process_Refund;
    begin
      With CompleteJournalEntry do
        begin
        with _JournalDetailEntries[0] do
         begin
           TransNo := TN;
 //          AcctNo :=
           AcctGuid := StringToGUID(GUID_UNCAT_EXP);// Uncategorized Expense
           TransRow := 0;
           Text := self.data.Memo;
           // This should really multiply the amount depending on the currency
           // f.e. Amount := AMT * 100 for USD
           Amount :=  Abs(FloatToDBAmount('JPY',  self.Data.LocalCurrencyAmount));
           DrCr:=Cr;
         end;
 // Credit Card
       with _JournalDetailEntries[1] do
         begin
                     TransNo := TN;
           //          AcctNo :=
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
        with _JournalDetailEntries[0] do
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
       with _JournalDetailEntries[1] do
         begin
                     TransNo := TN;
           //          AcctNo :=
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
    end;

  // Process an eentry with a negative amount.
  // This is currentlt the same as a positive amount except that we make the
  // amounts positive and then swap the debit and credit to compensate.
  // (Note: This makes sense for reverals on the card, but not for card payments.)
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
       with _JournalDetailEntries[0] do
        begin
          TransNo := TN;
          If ImportMapEntry.Load(self.InterfaceGUID, self.data.Memo) then
            begin
              AcctGuid := ImportMapEntry.AcctGUID;   // Actual Mapped Account
            end
          else
            AcctGuid := StringToGUID(GUID_UNCAT_EXP);// Uncategorized Expense
          TransRow := 0;
          Text := self.data.Memo;
          // This should really multiply the amount depending on the currency
          // f.e. Amount := AMT * 100 for USD
          Amount :=  FloatToDBAmount('JPY',  self.Data.LocalCurrencyAmount);
          DrCr:=Dr;
        end;
// Credit Card
      with _JournalDetailEntries[1] do
        begin
                    TransNo := TN;
          //          AcctNo :=
                    AcctGuid := StringToGUID(GUID_AMEX_JAPAN); // Amex Japan
                    TransRow := 1;
                    Text := self.data.Memo;
                    // This should really multiply the amount depending on the currency
                    // f.e. Amount := AMT * 100 for USD
                    Amount := FloatToDBAmount('JPY',
                                              self.Data.LocalCurrencyAmount);
                    DrCr:=Cr;
        end;
       end;
    end; // of [sub]PROCEDURE

  begin
    With CompleteJournalEntry do
    begin
      TN := HighWaterMark + 1;
      With _JournalHeader do
        begin
          HdrTransNo := TN;
 //         CurrCode  := 'JPY'; // This is Amex JAPAN
          EffDate   := self.Data.TransactionDate;
          HdrMemo   := self.Data.Memo;
          HdrPosted := True;
//          Commit;
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

  If Insert then
      Writeln(MSGTRANSINSSUCC) //'Transaction Inserted.')
  else
     Raise Exception.Create(MSGTRANSINSERR);//'Error Inserting Transaction!');

    end; // of with CompleteJournalEntry

  end;

Destructor TAmexJPCSVImport.Destroy();
  begin
    _CSVImport.free();
  end;

Initialization
  ShortDateFormat := 'YYYY/MM/DD';
  CSVImporter := TCSVImport.Create();

finalization
  CSVImporter.Free();

end.

