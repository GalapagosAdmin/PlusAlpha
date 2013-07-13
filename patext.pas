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
     _TextGUID : TGUID;
     _HasGUID:Boolean;
     _LangCD : TLangCode;
     _LangCode_Fallback:TLangCode;
     _Text : UTF8String;
     Function Select:Boolean;
     Function Insert:Boolean;
     Function Update:Boolean;
   public
     Constructor Create; //overload;
     Property Language : TLangCode read _LangCd;
     Property Text : UTF8String Read _Text;
     Function GetText(TextCd:Integer):UTF8String;
     Function GetText(TextGUID:TGUID):UTF8String;
 end;


 Var
   DBText:TText;

implementation

  uses paDatabase, sqldb, LazUTF8;
  ResourceString
    ErrCantReadText='Error: Unable to load requested text item.';

 Constructor TText.create;
  Function GetShortLanguageID:TLangCode;
    var
      tmp:String;
    begin
      LazGetShortLanguageID(tmp);
      Result := UpperCase(copy(tmp,1,2));
    end;

   begin
     _LangCode_Fallback := 'EN';
//     _LangCd := GetEnvironmentVariable('LANG');
     _LangCD := GetShortLanguageID;
     _TextCD := 0;
     CreateGUID(_TextGUID);
     inherited create;
   end;

 Function TText.Select:boolean;
 var
   SQLQuery1:TSQLQuery;
  begin
    try
    SQLQuery1 := TSQLQuery.Create(nil);

  //Journal Header should always be inserted first, so it's safer to take that
  // number
  With SQLQuery1 do
    begin
      Transaction := SQLTransaction1;
      IF _HasGUID then
        begin  // Use Text GUID
          SQL.Text := 'SELECT Text from Text Where TextGUID = :TextGUID and LangCD = :LangCd';
          ParamByName('TextGUID').AsString := GuidToString(_TextGUID);
        end
      else     // Use Text Code
        begin
          SQL.Text := 'SELECT Text from Text Where TextCD = :TextCD and LangCD = :LangCd';
          ParamByName('TextCD').AsInteger := _TextCD;
        end;
      ParamByName('LangCD').AsString := _LangCD;

      Open;
      If not EOF then
        begin
          _Text := FieldByName('text').AsString;
          Result := True;
        end
      else
        begin
          // backup plan - Try with Fallback language
         ParamByName('LangCD').AsString := _LangCode_Fallback;
         if SQLQuery1.Active then SQLQuery1.close;
         Open;
         If not EOF then
           begin
             _Text := FieldByName('text').AsString;
             Result := True;
           end
         else
           raise exception.Create(ErrCantReadText);
        end;
      Close;
      Destroy;
    end;
    except
      raise exception.Create('Error in paText.TText.select');
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
     _Text := '';
     _TextCd := TextCd;
     _HasGUID := False;
     if select then
       Result := _Text
     else
       result := '';
   end;

 Function TText.GetText(TextGUID:TGUID):UTF8String;
   begin
     _Text := '';
     _TextCd := -1;
     _TextGUID := TextGUID;
     _HasGUID := True;
     if select then
       Result := _Text
     else
       result := '';
   end;


initialization
  DBText := TText.create();

finalization
  DBText.Free;

end.

