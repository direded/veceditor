unit UPaintSpace;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, FileUtil, Forms, Graphics, Dialogs, Menus,
  ExtCtrls, UDoublePoint, Math, UUtils;

type

  TPaintSpace = class
  private
    FPaintBox: TPaintBox;
    FLocalSpace: TDoubleRect;
    FOriginalSize: TDoublePoint;
    FWorldSpace: TDoubleRect;
    FFiguresBounds: TDoubleRect;
    FScaleChangeEvent: TEventHandler;
    FPositionChangeEvent: TEventHandler;
    FWorldSpaceChangeEvent: TEventHandler;
    procedure SetScale(AValue: Double);
    function GetScale: Double;
    function GetPosition: TDoublePoint;
    function GetCanvas: TCanvas;
    function GetPaintBox: TPaintBox;
  public
    property Canvas: TCanvas read GetCanvas;
    property PaintBox: TPaintBox read GetPaintBox;
    procedure SetPosForced(AValue: TDoublePoint);
    procedure SetPosSafely(AValue: TDoublePoint);
    property Position: TDoublePoint read GetPosition write SetPosForced;
    property Scale: Double read GetScale write SetScale;
    property LocalSpace: TDoubleRect read FLocalSpace;
    property WorldSpace: TDoubleRect read FWorldSpace;
    property FiguresBounds: TDoubleRect read FFiguresBounds;
    property OriginalSize: TDoublePoint read FOriginalSize;
    property OnScaleChange: TEventHandler read FScaleChangeEvent write FScaleChangeEvent;
    property OnPositionChange: TEventHandler read FPositionChangeEvent write FPositionChangeEvent;
    property OnWorldSpaceChange: TEventHandler read FWorldSpaceChangeEvent write FWorldSpaceChangeEvent;
    constructor Create(APaintBox: TPaintBox; AOriginalSize: TDoublePoint);
    procedure SetFiguresBounds(AMin, AMax: TDoublePoint);
    procedure SetScaleCenter(AValue: Double);
    procedure SetScalePoint(AValue: Double; APoint: TDoublePoint);
		function ToWorld(APoint: TDoublePoint): TDoublePoint;
  	function ToLocal(APoint: TDoublePoint): TPoint;
    function ToLocal(APointArray: TDoublePointArray): TPointArray;
  const MAX_SCALE: Integer = 15;
  end;

  function DoubleRect(AWidth, AHeight: Double): TDoubleRect;

implementation

constructor TPaintSpace.Create(APaintBox: TPaintBox; AOriginalSize: TDoublePoint);
begin
  FPaintBox:= APaintBox;
  FOriginalSize:= AOriginalSize;
  FLocalSpace.Width:= FPaintBox.Width;
  FLocalSpace.Height:= FPaintBox.Height;
  FLocalSpace.TopLeft:= GetDoublePoint;
  FWorldSpace.Width:= FOriginalSize.X;
  FWorldSpace.Height:= FOriginalSize.Y;
  FWorldSpace.TopLeft:= GetDoublePoint;
end;

procedure TPaintSpace.SetFiguresBounds(AMin, AMax: TDoublePoint);
begin
  FFiguresBounds.TopLeft:= AMin;
  FFiguresBounds.Width:= AMax.X - FFiguresBounds.TopLeft.X;
  FFiguresBounds.Height:= AMax.Y - FFiguresBounds.TopLeft.Y;
  SetPosForced(Position);
end;

function TPaintSpace.GetCanvas: TCanvas;
begin
  Result:= FPaintBox.Canvas;
end;

procedure TPaintSpace.SetScaleCenter(AValue: Double);
var
  TempPoint: TDoublePoint;
begin
  if (AValue <= 0) or (AValue > MAX_SCALE) then exit;
  TempPoint:= GetDoublePoint(FPaintBox.Width, FPaintBox.Height)/2;
  Position:= Position+TempPoint/Scale;
  Scale:= AValue;
  Position:= Position-TempPoint/Scale;
end;

procedure TPaintSpace.SetScalePoint(AValue: Double; APoint: TDoublePoint);
var
  TempPoint: TDoublePoint;
begin
  if (AValue <= 0) or (AValue > MAX_SCALE) then Exit;
  TempPoint:= APoint-Position*Scale;
  Position:= Position+TempPoint/Scale;
  Scale:= AValue;
  Position:= Position-TempPoint/Scale;
end;

procedure TPaintSpace.SetPosForced(AValue: TDoublePoint);
var
  MinBound, MaxBound: TDoublePoint;
begin
  MinBound.X:= Min(FFiguresBounds.TopLeft.X, 0);
  MinBound.Y:= Min(FFiguresBounds.TopLeft.Y, 0);
  MaxBound.X:= Max(FFiguresBounds.TopLeft.X+FFiguresBounds.Width, FOriginalSize.X);
  MaxBound.Y:= Max(FFiguresBounds.TopLeft.Y+FFiguresBounds.Height, FOriginalSize.Y);

  FLocalSpace.TopLeft:= AValue;
  FWorldSpace.TopLeft.X:= Min(FLocalSpace.TopLeft.X, MinBound.X);
  FWorldSpace.TopLeft.Y:= Min(FLocalSpace.TopLeft.Y, MinBound.Y);
  FWorldSpace.Width:= Max(FLocalSpace.TopLeft.X+FLocalSpace.Width, MaxBound.X)-FWorldSpace.TopLeft.X;
  FWorldSpace.Height:= Max(FLocalSpace.TopLeft.Y+FLocalSpace.Height, MaxBound.Y)-FWorldSpace.TopLeft.Y;

  if Assigned(FPositionChangeEvent) then
    FPositionChangeEvent;
end;

procedure TPaintSpace.SetPosSafely(AValue: TDoublePoint);
var
  MinBound, MaxBound: TDoublePoint;
begin
  FWorldSpace.Height:= Max(FLocalSpace.TopLeft.Y+FLocalSpace.Height, MaxBound.Y)-FWorldSpace.TopLeft.Y;
  if (AValue.X < FWorldSpace.TopLeft.X) or
    (AValue.Y < FWorldSpace.TopLeft.Y) or
    (AValue.X+FLocalSpace.Width > FWorldSpace.Width) or
    (AValue.Y+FLocalSpace.Height > FWorldSpace.Height) then
    Exit;
  FLocalSpace.TopLeft:= AValue;
  if Assigned(FPositionChangeEvent) then
    FPositionChangeEvent;
end;

procedure TPaintSpace.SetScale(AValue: Double);
begin
  if (AValue <= 0) or (AValue > MAX_SCALE) then Exit;
  FLocalSpace.Width:= FPaintBox.Width/AValue;
  FLocalSpace.Height:= FPaintBox.Height/AValue;
  if Assigned(FScaleChangeEvent) then
    FScaleChangeEvent;
end;

function TPaintSpace.GetScale: Double;
begin
  Result:= FPaintBox.Width/FLocalSpace.Width;
end;

function TPaintSpace.GetPosition: TDoublePoint;
begin
  Result:= GetDoublePoint(FLocalSpace.TopLeft.X, FLocalSpace.TopLeft.Y);
end;

function TPaintSpace.GetPaintBox: TPaintBox;
begin
  Result:= FPaintBox;
end;

function TPaintSpace.ToWorld(APoint: TDoublePoint): TDoublePoint;
begin
	Result:= APoint/Scale + Position;
end;

function TPaintSpace.ToLocal(APoint: TDoublePoint): TPoint;
begin
  Result:= ((APoint - Position) * Scale).ToPoint;
end;

function TPaintSpace.ToLocal(APointArray: TDoublePointArray): TPointArray;
var
  i: Integer;
begin
  SetLength(Result, Length(APointArray));
  for i:= 0 to High(APointArray) do
    Result[i]:= ToLocal(APointArray[i]);
end;

function DoubleRect(AWidth, AHeight: Double): TDoubleRect;
begin
  Result.Width:= AWidth;
  Result.Height:= AHeight;
end;

end.

