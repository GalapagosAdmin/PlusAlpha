program PlusAlpha;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, paFrmMainUnit, libpa, padm, sdflaz, lazcontrols, anchordockpkg,
  frmTransactionUnit, frmDebugUnit, frmLedgerUnit, paLedger, paDatabase,
  pacalculator, paJournal, paText, paTransactionList, frmTransactionListUnit,
  fmeWelcomeUnit, paMessage;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFormPlusAlphaMain, FormPlusAlphaMain);
  Application.CreateForm(TfrmTransaction, frmTransaction);
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(TfrmDebug, frmDebug);
  Application.CreateForm(TfrmTransactionList, frmTransactionList);
  Application.Run;
end.

