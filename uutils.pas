unit UUtils;

{$mode objfpc}{$H+}

interface

uses
  UDoublePoint;

type

  TIntArray = array of Integer;

  TEventHandler = procedure of object;

  TDoubleRect = record
    Width, Height: Double;
    TopLeft: TDoublePoint;
  end;

  function IsInArray(Element: Integer; var Arr: TIntArray): Boolean;
  procedure Push(Element: Integer; Arr: TIntArray);

implementation

function IsInArray(Element: Integer; var Arr: TIntArray): Boolean;
var
  i: Integer;
begin
  for i:= Low(Arr) to High(Arr) do
    if Arr[i] = Element then Exit(true);
  Result:= false;
end;

procedure Push(Element: Integer; Arr: TIntArray);
begin
  SetLength(Arr, Length(Arr)+1);
  Arr[High(Arr)]:= Element;
end;

end.

