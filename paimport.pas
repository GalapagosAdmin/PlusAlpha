unit paImport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  CSVDocument, // CSVDocument is separate from PlusAlpha project.
  paCurrency;

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
  end;

  TAmexJPEntry=Record
    TransactionDate:TDate;
    LocalCurrencyAmount:Real;
    Memo:UTF8String;
    ForeignCurrencyAmount:Real;
    ForeignCurrencyCode:TCurrCode;
  end;

  TAmexJPCSVImport = class(TObject)
    FDoc: TCSVDocument;
    RowCount:Integer;
    ColCount:Integer;
    _CSVImport:TCSVImport;
    _CurrentRow:Integer;
    _EOF:Boolean;
    _CurrentEntry:TAmexJPEntry;
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
  var
    TN:Integer;
  begin
    With CompleteJournalEntry do
    begin
      TN := HighWaterMark+1;

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
        begin
         // Convert amount to positive amount, and reverse Dr/Cr
         // Change accounts ?
         //
        end;

// Debit Expense
      with _JournalDetailEntries[0] do
        begin
          TransNo := TN;
//          AcctNo :=
          AcctGuid := StringToGUID('{22EF5AE8-FD0D-49E5-945E-C88C8B5AA599}');// Uncategorized Expense
          TransRow := 0;
          Text := self.data.Memo;
          // This should really multiply the amount depending on the currency
          // f.e. Amount := AMT * 100 for USD
          Amount :=  FloatToDBAmount('JPY',  self.Data.LocalCurrencyAmount));
          DrCr:=Dr;
        end;
// Credit Card
      with _JournalDetailEntries[1] do
        begin
                    TransNo := TN;
          //          AcctNo :=
                    AcctGuid := StringToGUID('{4D1ABE22-2073-4779-A46B-A92DC6C46D23}'); // Amex Japan
                    TransRow := 1;
                    Text := self.data.Memo;
                    // This should really multiply the amount depending on the currency
                    // f.e. Amount := AMT * 100 for USD
                    Amount := FloatToDBAmount('JPY',
                                              self.Data.LocalCurrencyAmount);
                    DrCr:=Cr;
        end;

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

