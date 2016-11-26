unit UUtils;

{$mode objfpc}{$H+}

interface

uses
  UDoublePoint;

type

  TEventHandler = procedure of object;

  TDoubleRect = record
    Width, Height: Double;
    TopLeft: TDoublePoint;
  end;

implementation

end.

