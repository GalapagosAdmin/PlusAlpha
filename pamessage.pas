unit paMessage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Procedure DisplayMessage(Const Message:UTF8String);

implementation

Uses
  paFrmMainUnit;

Procedure DisplayMessage(Const Message:UTF8String);
  begin
    FormPlusAlphaMain.Statusbar1.simpletext := Message;
  end;

end.

