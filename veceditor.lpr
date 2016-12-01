program veceditor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,  // this includes the LCL widgetset
  Forms, Main, About, Utools, UFigures, UDoublePoint, UPaintSpace, UUtils,
  UFigureParams, utoolparams;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TaboutForm, aboutForm);
  Application.Run;
end.

