
program c4;

{.$APPTYPE GUI}
{$MODE OBJFPC}{$H+}

uses
  Interfaces, Forms, main;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
