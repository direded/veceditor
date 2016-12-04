unit UGeometry;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UDoublePoint, Math;

  function IsPointInLineSegment(A, B, P: TDoublePoint; Eps: Double = 0): Boolean;

implementation

function IsPointInLineSegment(A, B, P: TDoublePoint; Eps: Double = 0): Boolean;
begin
  if (abs(GetVecMultiplyLength(P-A, B-A)) <= Eps)  and
      (Min(A.X, B.X) <= P.X) and (P.X <= Max(A.X, B.X)) then Exit(true);
  Exit(false);
end;

end.

