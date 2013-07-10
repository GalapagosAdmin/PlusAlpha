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
    Procedure SetFileName(Const FileName:UTF8String);
    Property Data:TAmexJPEntry read _CurrentEntry;
    Procedure GetNext;
    property EOF:Boolean Read _EOF;
//    Function GetValue(const aCol:Integer; aRow:Integer):UTF8String;
//    Function GetRowCount:Integer;
  end;


var
  CSVImporter:TCSVImport;


implementation

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
    Result := ANSItoUTF8(Result);
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
    _CurrentEntry.TransactionDate:= StrToDate(_CSVImport.GetValue(DateCol,_CurrentRow));
    // Remove Quotation Marks, Commas if needed, convert to Integer
    tmpAmt :=
      StringReplace(_CSVImport.GetValue(LocalCurrencyAmtCol,_CurrentRow),',','',[rfReplaceAll]);
    _CurrentEntry.LocalCurrencyAmount := StrToFloat(tmpAmt);
    // Remove Quotation Chars, Convert Encoding to UTF8 if required
    _CurrentEntry.Memo:=  ANSIToUTF8(_CSVImport.GetValue(MemoCol,_CurrentRow));
    // Remove Quotation Marks
    // Split last field into Currency Amount and Currency Code
    tmpFC := _CSVImport.GetValue(ForeignCurrencyCol,_CurrentRow);
    If tmpFC = '' then
      begin
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

