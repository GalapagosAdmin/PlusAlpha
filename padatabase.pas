unit paDatabase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  sqldb, sqlite3conn;

var
        DatabaseDir:UTF8String;
        DatabaseName:UTF8String;
        CompleteDBPath:UTF8String;
        // Connection to be shared among other objects
        SQLite3Connection1:TSQLite3Connection;
        SQLTransaction1:TSQLTransaction;

implementation

uses
  FileUtil;

initialization

  DatabaseName := 'plusalpha.sqlite';
  SQLite3Connection1 := TSQLite3Connection.Create(nil);
  // The following returns ~/.config/PlusAlpha/ on OS X
  DatabaseDir := GetAppConfigDirUTF8(False);
  CompleteDBPath := DatabaseDir + DatabaseName;
  SQLite3Connection1.DatabaseName := CompleteDBPath;
  try
    SQLite3Connection1.Connected := True;
    SQLTransaction1 := TSQLTransaction.Create(nil);
    SQLTransaction1.DataBase := SQLite3Connection1;
  except
    raise Exception.Create('Database not found at: ' + CompleteDBPath);
  end;

end.

