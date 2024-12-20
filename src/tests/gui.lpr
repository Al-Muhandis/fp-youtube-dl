program gui;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, testyoutubedl, youtubedl;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

