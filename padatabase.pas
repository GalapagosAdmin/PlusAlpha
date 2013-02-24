unit paDatabase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  sqldb, sqlite3conn;

var
        // Connection to be shared among other objects
        SQLite3Connection1:TSQLite3Connection;
        SQLTransaction1:TSQLTransaction;

implementation
 //uses


initialization
  SQLite3Connection1 := TSQLite3Connection.Create(nil);
  SQLite3Connection1.DatabaseName :=  '/Users/shiruba/Develop/plusalpha/plusalpha.sqlite';
  SQLite3Connection1.Connected := True;
  SQLTransaction1 := TSQLTransaction.Create(nil);
  SQLTransaction1.DataBase := SQLite3Connection1;


end.

