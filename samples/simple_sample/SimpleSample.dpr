program SimpleSample;

uses
  Vcl.Forms,
  SimpleSample.Main in 'SimpleSample.Main.pas' {FMain},
  SQLCollection.Base in '..\..\sources\SQLCollection.Base.pas',
  SQLCollection.Core in '..\..\sources\SQLCollection.Core.pas',
  SQLCollection.Design.Editor in '..\..\sources\SQLCollection.Design.Editor.pas' {SQLCollectionEditor},
  SQLCollection.Design.SQLEditor in '..\..\sources\SQLCollection.Design.SQLEditor.pas' {SQLEditor};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFMain, FMain);
  Application.Run;
end.
