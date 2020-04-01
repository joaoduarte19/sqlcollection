unit SimpleSample.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, SQLCollection.Core, Vcl.StdCtrls;

type
  TFMain = class(TForm)
    SQLCollection1: TSQLCollection;
    btn1: TButton;
    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FMain: TFMain;

implementation

uses
  SQLCollection.Design.Editor;

{$R *.dfm}

procedure TFMain.btn1Click(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Pred(SQLCollection1.Items.Count) do
    SQLCollection1.Items.SQLCategory[I].SQLItems.Sort;

  SQLCollection1.Items.Sort;

  with TSQLCollectionEditor.Create(Self) do
  try

    SQLCollection := SQLCollection1;
    ShowModal;
  finally
    Release;
  end;
end;

end.
