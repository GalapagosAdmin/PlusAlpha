unit frmTransactionListUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Grids;

type

  { TfrmTransactionList }

  TfrmTransactionList = class(TForm)
    pnlToolbar: TPanel;
    StringGrid1: TStringGrid;
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmTransactionList: TfrmTransactionList;

implementation

uses paTransactionList;


{$R *.lfm}

{ TfrmTransactionList }

procedure TfrmTransactionList.FormShow(Sender: TObject);
var
  ResultEntry : TResultEntry;
begin
  TransactionList.Account := 3;
  With TransactionList do for ResultEntry in TransNos do
    begin
      StringGrid1.cols[0].Append(IntToStr( ResultEntry.TransactionNo));;
      StringGrid1.cols[1].Append(IntToStr( ResultEntry.TransactionRow));;
    end;
end;

end.

