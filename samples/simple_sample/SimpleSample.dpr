program SimpleSample;

uses
  Vcl.Forms,
  SimpleSample.Main in 'SimpleSample.Main.pas' {FMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFMain, FMain);
  Application.Run;
end.
