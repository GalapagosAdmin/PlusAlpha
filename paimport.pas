unit paImport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  csvdocument; // CSVDocument is separate from PlusAlpha project.

type
  TCSVImport = class(TObject)
    FDoc: TCSVDocument;
    RowCount:Integer;
    ColCount:Integer;
   public
    Constructor create();
    Destructor Destroy();
    Procedure SetFileName(Const FileName:UTF8String);
    Function GetValue(const aCol:Integer; aRow:Integer):UTF8String;
    Function GetRowCount:Integer;
  end;

var
  CSVImporter:TCSVImport;


implementation


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
    FDoc.free;
  end;

Initialization
  CSVImporter := TCSVImport.create();


finalization
  CSVImporter.free();


end.

