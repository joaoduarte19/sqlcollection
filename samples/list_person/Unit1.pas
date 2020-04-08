unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.VCLUI.Wait,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Phys.PGDef, Data.DB, SQLCollection.Core,
  Vcl.StdCtrls, Vcl.Grids, Vcl.DBGrids, FireDAC.Phys.PG, FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type
  TForm1 = class(TForm)
    fdConnection: TFDConnection;
    qryPerson: TFDQuery;
    dsPerson: TDataSource;
    FDPhysPgDriverLink1: TFDPhysPgDriverLink;
    dbgrd1: TDBGrid;
    btn1: TButton;
    SQLCollection1: TSQLCollection;
    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btn1Click(Sender: TObject);
begin
  qryPerson.SQL.Text := SQLCollection1.FindSQL('Select person', 'Person');
  qryPerson.Open;
end;

end.
