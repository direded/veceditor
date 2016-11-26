unit UDoublePoint;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

type

	TDoublePoint = object
  private
    function GetAngle: Double;
    procedure SetAngle(AAngle: Double);
  public
    X, Y: Double;
    property Angle: Double read GetAngle write SetAngle;
		function Length: Double;
    function GetNormalized: TDoublePoint;
    function ToPoint: TPoint;
    function ToString: String;
    procedure Rotate(AAngle: Double);
		procedure Normalize;
  end;
  TPointArray = array of TPoint;
  TDoublePointArray = array of TDoublePoint;
  function GetPointArray(ADblPointArray: TDoublePointArray): TPointArray;
  operator +(A: TDoublePoint; B: TDoublePoint): TDoublePoint;
  operator -(A: TDoublePoint; B: TDoublePoint): TDoublePoint;
  operator -(A: TDoublePoint): TDoublePoint;
  operator *(A: Double; B: TDoublePoint): TDoublePoint;
	operator *(A: TDoublePoint; B: Double): TDoublePoint;
  operator /(A: TDoublePoint; B: Double): TDoublePoint;
  operator =(A: TDoublePoint; B: TDoublePoint): Boolean;
  operator =(A: TPoint; B: TPoint): Boolean;
  operator :=(A: TPoint): TDoublePoint;

  function GetDoublePoint(AX: Double = 0; AY: Double = 0): TDoublePoint;
  function PointToDoublePoint(APoint: TPoint): TDoublePoint;

implementation

function TDoublePoint.GetAngle: Double;
begin
	Result:= arctan2(Y, X);
end;

function TDoublePoint.Length: Double;
begin
	Result:= sqrt(sqr(X)+sqr(Y));
end;

function TDoublePoint.GetNormalized: TDoublePoint;
begin
  Result:= Self;
  Result.Normalize;
end;

function TDoublePoint.ToPoint: TPoint;
begin
  Result.X:= Round(X);
  Result.Y:= Round(Y);
end;

function TDoublePoint.ToString: String;
begin
  Result:= '( ' + IntToStr(round(X)) + ' ; ' + FloatToStr(round(Y)) + ' )';
end;

procedure TDoublePoint.setAngle(AAngle: Double);
begin
	Rotate(AAngle-getAngle);
end;

procedure TDoublePoint.Rotate(AAngle: Double);
var
  TempX, TempY: Extended;
begin
	TempX:= X*cos(AAngle)-Y*sin(AAngle);
  TempY:= X*sin(AAngle)+Y*cos(AAngle);
  X:= TempX; Y:= TempY;
end;

procedure TDoublePoint.Normalize;
begin
  X:= X / Length;
  Y:= Y / Length;
end;

function GetPointArray(ADblPointArray: TDoublePointArray): TPointArray;
var
  i: Integer;
begin
  SetLength(Result, Length(ADblPointArray));
  for i:= 0 to High(Result) do
    Result[i]:= ADblPointArray[i].ToPoint;
end;

operator +(A: TDoublePoint; B: TDoublePoint): TDoublePoint;
begin
  Result.X:= A.X+B.X;
  Result.Y:= A.Y+B.Y;
end;

operator -(A: TDoublePoint; B: TDoublePoint): TDoublePoint;
begin
  Result.X:= A.X-B.X;
	Result.Y:= A.Y-B.Y;
end;

operator -(A: TDoublePoint): TDoublePoint;
begin
  Result.X:= -A.X;
  Result.Y:= -A.Y;
end;

operator *(A: Double; B: TDoublePoint) c: TDoublePoint;
begin
  Result.X:= A*B.X;
  Result.Y:= A*B.Y;
end;

operator *(A: TDoublePoint; B: Double): TDoublePoint;
begin
  Result.X:= B*A.X;
  Result.Y:= B*A.Y;
end;

operator /(A: TDoublePoint; B: Double): TDoublePoint;
begin
  Result.X:= A.X/B;
  Result.Y:= A.Y/B;
end;

operator =(A: TDoublePoint; B: TDoublePoint): Boolean;
  begin
    Result:= (A.X = B.X) and (A.Y = B.Y);
  end;

operator =(A: TPoint; B: TPoint): Boolean;
begin
	Result:= (A.X = B.X) and (A.Y = B.Y);
end;

operator :=(A: TPoint): TDoublePoint;
begin
  Result.X:= A.X;
  Result.Y:= A.Y;
end;

function GetDoublePoint(AX: Double = 0; AY: Double = 0): TDoublePoint;
begin
  Result.X:= AX; Result.Y:= AY;
end;


function PointToDoublePoint(APoint: TPoint): TDoublePoint;
begin
  Result.X:= APoint.X;
  Result.Y:= APoint.Y;
end;

end.

