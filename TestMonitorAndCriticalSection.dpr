program TestMonitorAndCriticalSection;

uses
  Vcl.Forms,
  TestFrm in 'TestFrm.pas' {FormTest};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormTest, FormTest);
  Application.Run;
end.
