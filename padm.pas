unit padm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SdfData, sqlite3conn;

type

  { TDataModule1 }

  TDataModule1 = class(TDataModule)
    SdfDataSet1: TSdfDataSet;
    SQLite3Connection1: TSQLite3Connection;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  DataModule1: TDataModule1;

implementation

{$R *.lfm}

end.

