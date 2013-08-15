unit paImport;
{$mode objfpc}{$H+}
{$INTERFACES CORBA }
interface

uses
  Classes, SysUtils,
  CSVDocument, // CSVDocument is separate from PlusAlpha project.
  paCurrency, md5,
  paImportMap;

ResourceString
  ERRNOSUCHROW = 'Invalid Row Reference reading CSV file.';

Const
    SImportInterface = '{F3FBC2C5-089C-438E-8FE7-C80E0E3322CB}';

Type
   // This is for actual import classes
   IImportInterface = interface
    [SImportInterface]
    Procedure SetFileName(Const FileName:UTF8String);
    // ProcedureGetFirst; // Rewind to the beginning
    Procedure GetNext;
    property EOF:Boolean;
    Property TestMode:Boolean;
    Procedure CreateTransaction;
  end;

  // This one is just a thin wrapper around TCSVDocument
  // Using it instead of a class helper because we may reimplement this later
  TCSVImport = class(TObject)
    FDoc: TCSVDocument;
    RowCount:Integer;
    ColCount:Integer;
    SourceEncodingIsUTF8:Boolean;
  public
    Constructor Create();
    Destructor Destroy();
    Procedure SetFileName(Const FileName:UTF8String);
    Function GetValue(const aCol:Integer; aRow:Integer):UTF8String;
    Function GetRowCount:Integer;
    Function GetRowMD5Hash(Const Row:Integer):TMD5Digest;
  end;

var
  CSVImporter:TCSVImport;


Implementation

Uses
 paJournal, LibPa;



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
    if not SourceEncodingIsUTF8 then Result := ANSIToUTF8(Result);
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
    ShortDateFormat := 'YYYY/MM/DD'; // Doesn't seem to take effect on all systems
    SourceEncodingIsUTF8 := False;
  end;

Destructor TCSVImport.Destroy();
  begin
    inherited;
    FDoc.free;
  end;


Initialization
  CSVImporter := TCSVImport.Create();

finalization
  CSVImporter.Free();

end.

