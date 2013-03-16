unit paText;
// This unit gets/sets internationalized text using the database

{$mode objfpc}{$H+}

// Table Name: Text
// Table Fields:
// TextCD = Text Code (Integer)
// LangCD = 2 char Language Code
// Text = Actual text in UTF8

interface

 uses
  Classes, SysUtils,
   Libpa;

 Type TText=Class(TObject)
   Private
     _TextCD : Integer;
     _LangCD : TLangCode;
     _Text : UTF8String;
     Function Select:Boolean;
     Function Insert:Boolean;
     Function Update:Boolean;
   public
     Constructor Create; //overload;
     Property Language : TLangCode read _LangCd;
     Property Text : UTF8String Read _Text;
     Function GetText(TextCd:Integer):UTF8String;
 end;


 Var
   DBText:TText;

implementation

  uses paDatabase, sqldb;

 Constructor TText.create;
   begin
     _LangCd := 'EN';
     _TextCd := 0;
     inherited create;
   end;

 Function TText.Select:boolean;
 var
   SQLQuery1:TSQLQuery;
  begin
    SQLQuery1 := TSQLQuery.Create(nil);

  //Journal Header should always be inserted first, so it's safer to take that
  // number
  With SQLQuery1 do
    begin
      Transaction := SQLTransaction1;
      SQL.Text := 'SELECT Text from Text Where TextCD = :TextCD and LangCD = :LangCd';
      ParamByName('TextCD').AsInteger := _TextCD;
      ParamByName('LangCD').AsString := _LangCD;

      Open;
      If not EOF then
        _Text := FieldByName('text').AsString;
      Close;
      Destroy;
    end;
  end;


 Function TText.Insert:boolean;
   begin
   end;

 Function TText.Update:boolean;
   begin
   end;

 Function TText.GetText(TextCd:Integer):UTF8String;
   begin
     _TextCd := TextCd;
     select;
     Result := _Text
   end;



initialization
  DBText := TText.create();


end.

