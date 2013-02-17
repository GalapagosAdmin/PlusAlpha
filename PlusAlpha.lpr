program PlusAlpha;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, paFrmMainUnit, libpa, padm, sdflaz, frmTransactionUnit, frmDebugUnit
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFormPlusAlphaMain, FormPlusAlphaMain);
  Application.CreateForm(TfrmTransaction, frmTransaction);
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(TfrmDebug, frmDebug);
  Application.Run;
end.

