program rjson_test;

uses
  System.StartUpCopy,
  FMX.Forms,
  FormMainUnit in 'FormMainUnit.pas' {FormMain},
  rjson in '..\rjson.pas';

{$R *.res}


begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;

end.
