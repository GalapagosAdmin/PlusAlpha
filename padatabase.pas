Unit paDatabase;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils,  sqldb, sqlite3conn;

Var
        DatabaseDir:UTF8String;
        DatabaseName:UTF8String;
        CompleteDBPath:UTF8String;
        // Connection to be shared among other objects
        SQLite3Connection1:TSQLite3Connection;
        SQLTransaction1:TSQLTransaction;

        Function CompleteDBPathGet:UTF8String;


Implementation

Uses
  FileUtil;

Function CompleteDBPathGet:UTF8String;
  begin
    Result := CompleteDBPath;
  end;

 Function GetApplicationName:string;//TGetAppNameEvent;
   begin
     Result := 'PlusAlpha';
   end;

ResourceString
  ERRDBNOTFOUND = 'Database not found at: ';

initialization
  OnGetApplicationName := @GetApplicationName;
  DatabaseName := 'plusalpha.sqlite';
  SQLite3Connection1 := TSQLite3Connection.Create(nil);
  // The following returns ~/.config/PlusAlpha/ on OS X
  // Global = False means a per-user config directory
  DatabaseDir := GetAppConfigDirUTF8(False);
  CompleteDBPath := DatabaseDir + DatabaseName;
  SQLite3Connection1.DatabaseName := CompleteDBPath;
  try
    SQLite3Connection1.Connected := True;
    SQLTransaction1 := TSQLTransaction.Create(nil);
    SQLTransaction1.DataBase := SQLite3Connection1;
  except
    raise Exception.Create(ERRDBNOTFOUND+ CompleteDBPath);
  end;

end.

