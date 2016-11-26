unit UFigures;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, Controls, FileUtil, Forms, Graphics, Dialogs, Menus,
  ExtCtrls, Math, UDoublePoint, UPaintSpace, FPCanvas, UUtils, UFigureParams;

type

  TTwoDoublePointsArray = array[0..1] of TDoublePoint;

  TFigure = class
  strict protected
    FBounds: TTwoDoublePointsArray;
    FPoints: TDoublePointArray;
  public
    property Points: TDoublePointArray read FPoints write FPoints;
    property Bounds: TTwoDoublePointsArray read FBounds;
    constructor Create;
    function IsValid: Boolean; virtual; abstract;
    procedure SetPointsLength(ALength: Integer);
    procedure IncreasePointsLength;
    procedure Bake; virtual;
    procedure Draw(APaintSpace: TPaintSpace); virtual; abstract;
  end;

  TFigureArray = array of TFigure;

  TLineFigure = class(TFigure)
  strict protected
    FPenParams: TPenParams;
  public
    property PenParams: TPenParams read FPenParams write FPenParams;
    constructor Create;
    function IsValid: Boolean; override;
    procedure Draw(APaintSpace: TPaintSpace); override;
  end;

  TShapeFigure = class(TLineFigure)
  strict protected
    FBrushParams: TBrushParams;
  public
    constructor Create;
    property BrushParams: TBrushParams read FBrushParams write FBrushParams;
    function IsValid: Boolean; override;
  end;

  TRectFigure = class(TShapeFigure)
  public
    constructor Create;
    procedure Draw(APaintSpace: TPaintSpace); override;
  end;

  TPolygonFigure = class(TShapeFigure)
  public
    constructor Create;
    function IsValid: Boolean; override;
    procedure Draw(APaintSpace: TPaintSpace); override;
  end;

  TRectSplitOffFigure = class(TRectFigure)
  public
    constructor Create;
    procedure Draw(APaintSpace: TPaintSpace); override;
  end;

  TEllipseFigure = class(TShapeFigure)
	public
    constructor Create;
    procedure Draw(APaintSpace: TPaintSpace); override;
  end;

  TRoundedRectFigure = class(TShapeFigure)
  private
    FRounding: Integer;
	public
    property Rounding: Integer read FRounding write FRounding;
    constructor Create;
    procedure Draw(APaintSpace: TPaintSpace); override;
  end;

  TFigures = class
 	strict private
    Content: TFigureArray;
    FFigureAddEvent: TEventHandler;
    FBounds: TDoubleRect;
    procedure SetBounds;
  public
    property OnFigureAdd: TEventHandler read FFigureAddEvent write FFigureAddEvent;
    constructor Create;
    procedure AddFigure(AFigure: TFigure);
    function RemoveFigure(AElementID: Longint): Boolean;
    function RemoveLastFigure: Boolean;
    procedure BakeLastFigure;
    procedure GetBounds(var AMin: TDoublePoint; var AMax: TDoublePoint);
  	procedure Draw(APaintSpace: TPaintSpace);
    destructor Destroy; override;
  end;

  procedure SetBounds(var ABounds: TTwoDoublePointsArray; APoint: TDoublePoint);

implementation

procedure SetBounds(var ABounds: TTwoDoublePointsArray; APoint: TDoublePoint);
begin
  if APoint.X < ABounds[0].X then
    ABounds[0].X:= APoint.X;
  if APoint.Y < ABounds[0].Y then
    ABounds[0].Y:= APoint.Y;
  if APoint.X > ABounds[1].X then
    ABounds[1].X:= APoint.X;
  if APoint.Y > ABounds[1].Y then
    ABounds[1].Y:= APoint.Y;
end;

constructor TFigure.Create;
begin
end;

procedure TFigure.IncreasePointsLength;
begin
  SetLength(FPoints, Length(FPoints)+1);
end;

procedure TFigure.SetPointsLength(ALength: Integer);
begin
  SetLength(FPoints, ALength);
end;

procedure TFigure.Bake;
var
  P: TDoublePoint;
begin
  FBounds[0]:= FPoints[0];
  FBounds[1]:= FPoints[0];
  for P in FPoints do
    SetBounds(FBounds, P);
end;

constructor TLineFigure.Create;
begin
  inherited Create;
end;

function TLineFigure.IsValid: Boolean;
begin
  Result:= (Length(FPoints)>=2);
end;

procedure TLineFigure.Draw(APaintSpace: TPaintSpace);
var
  P: TDoublePoint;
  CanvasPenParams: TPenParams;
begin
  with APaintSpace do begin
    CanvasPenParams:= GetParams(Canvas.Pen);
    SetParams(FPenParams, Canvas.Pen);
  	Canvas.MoveTo(ToLocal(FPoints[0]));
		for P in FPoints do
  		Canvas.LineTo(ToLocal(P));
    SetParams(CanvasPenParams, Canvas.Pen);
  end;
end;

constructor TShapeFigure.Create;
begin
  inherited Create;
end;

function TShapeFigure.IsValid: Boolean;
begin
  Result:= (FPoints[0].X <> FPoints[1].X) and (FPoints[0].Y <> FPoints[1].Y);
end;

constructor TRectFigure.Create;
begin
  inherited Create;
end;

procedure TRectFigure.Draw(APaintSpace: TPaintSpace);
var
  CanvasPenParams: TPenParams;
  CanvasBrushParams: TBrushParams;
begin
  with APaintSpace do begin
    CanvasPenParams:= GetParams(Canvas.Pen);
    CanvasBrushParams:= GetParams(Canvas.Brush);
    SetParams(FPenParams, Canvas.Pen);
    SetParams(FBrushParams, Canvas.Brush);
    Canvas.Rectangle(ToLocal(FPoints[0]).X, ToLocal(FPoints[0]).Y, ToLocal(FPoints[1]).X, ToLocal(FPoints[1]).Y);
    SetParams(CanvasPenParams, Canvas.Pen);
    SetParams(CanvasBrushParams, Canvas.Brush);
  end;
end;

constructor TPolygonFigure.Create;
begin
  inherited Create;
end;

function TPolygonFigure.IsValid: Boolean;
begin
  Result:= FPoints[0] <> FPoints[1];
end;

procedure TPolygonFigure.Draw(APaintSpace: TPaintSpace);
var
  CanvasPenParams: TPenParams;
  CanvasBrushParams: TBrushParams;
begin
  with APaintSpace do begin
    CanvasPenParams:= GetParams(Canvas.Pen);
    CanvasBrushParams:= GetParams(Canvas.Brush);
    SetParams(FPenParams, Canvas.Pen);
    SetParams(FBrushParams, Canvas.Brush);
    Canvas.Polygon(ToLocal(Points));
    SetParams(CanvasPenParams, Canvas.Pen);
    SetParams(CanvasBrushParams, Canvas.Brush);
  end;
end;

constructor TRectSplitOffFigure.Create;
begin
  inherited Create;
end;

procedure TRectSplitOffFigure.Draw(APaintSpace: TPaintSpace);
var
  lastPenStyle: TFPPenStyle;
begin
  with APaintSpace do begin
    lastPenStyle:= Canvas.Pen.Style;
    Canvas.Pen.Style:=psDash;
    Canvas.Frame(ToLocal(FPoints[0]).X, ToLocal(FPoints[0]).Y, ToLocal(FPoints[1]).X, ToLocal(FPoints[1]).Y);
    Canvas.Pen.Style:= lastPenStyle;
  end;
end;

constructor TEllipseFigure.Create;
begin
  inherited Create;
end;

procedure TEllipseFigure.Draw(APaintSpace: TPaintSpace);
var
  CanvasPenParams: TPenParams;
  CanvasBrushParams: TBrushParams;
begin
  with APaintSpace do begin
    CanvasPenParams:= GetParams(Canvas.Pen);
    CanvasBrushParams:= GetParams(Canvas.Brush);
    SetParams(FPenParams, Canvas.Pen);
    SetParams(FBrushParams, Canvas.Brush);
    Canvas.Ellipse(ToLocal(FPoints[0]).X, ToLocal(FPoints[0]).Y, ToLocal(FPoints[1]).X, ToLocal(FPoints[1]).Y);
    SetParams(CanvasPenParams, Canvas.Pen);
    SetParams(CanvasBrushParams, Canvas.Brush);
  end;
end;

constructor TRoundedRectFigure.Create;
begin
  inherited Create;
end;

procedure TRoundedRectFigure.Draw(APaintSpace: TPaintSpace);
var
  CanvasPenParams: TPenParams;
  CanvasBrushParams: TBrushParams;
begin
  with APaintSpace do begin
    CanvasPenParams:= GetParams(Canvas.Pen);
    CanvasBrushParams:= GetParams(Canvas.Brush);
    SetParams(FPenParams, Canvas.Pen);
    SetParams(FBrushParams, Canvas.Brush);
    Canvas.RoundRect(ToLocal(FPoints[0]).X, ToLocal(FPoints[0]).Y, ToLocal(FPoints[1]).X, ToLocal(FPoints[1]).Y, FRounding, FRounding);
    SetParams(CanvasPenParams, Canvas.Pen);
    SetParams(CanvasBrushParams, Canvas.Brush);
  end;
end;

constructor TFigures.Create;
begin
	Content:= nil
end;

procedure TFigures.AddFigure(AFigure: TFigure);
begin
  SetLength(Content, Length(Content)+1);
  Content[High(Content)]:= AFigure;
end;

procedure TFigures.SetBounds;
var
  Figure: TFigure;
  Min, Max: TDoublePoint;
begin
  if Content = nil then exit;
  Min:= Content[0].Bounds[0];
  Max:= Content[0].Bounds[1];
  for Figure in Content do begin
    if Min.X > Figure.Bounds[0].X then
      Min.X:= Figure.Bounds[0].X;
    if Min.Y > Figure.Bounds[0].Y then
      Min.Y:= Figure.Bounds[0].Y;
    if Max.X < Figure.Bounds[1].X then
      Max.X:= Figure.Bounds[1].X;
    if Max.Y < Figure.Bounds[1].Y then
      Max.Y:= Figure.Bounds[1].Y;
  end;
  Min.X:= Min.X-5; Min.Y:= Min.Y-5;
  Max.X:= Max.X+5; Max.Y:= Max.Y+5;
  FBounds.TopLeft:= Min;
  FBounds.Width:= Max.X-FBounds.TopLeft.X;
  FBounds.Height:= Max.Y-FBounds.TopLeft.Y;
end;

function TFigures.RemoveLastFigure: Boolean;
begin
  if(Length(Content) = 0) then Exit(false);
  Content[High(Content)].Free;
  SetLength(Content, Length(Content)-1);
end;

procedure TFigures.BakeLastFigure;
begin
  Content[High(Content)].Bake;
  SetBounds;
  if Assigned(FFigureAddEvent) then
    FFigureAddEvent;
end;

procedure TFigures.GetBounds(var AMin: TDoublePoint; var AMax: TDoublePoint);
begin
  AMin:= FBounds.TopLeft;
  AMax.X:= FBounds.Width+FBounds.TopLeft.X;
  AMax.Y:= FBounds.Height+FBounds.TopLeft.Y;
end;

function TFigures.RemoveFigure(AElementID: Longint): Boolean;
var
  i: Longint;
begin
  if(AElementID < Low(Content)) or (AElementID >= High(Content)) then Exit(false);
	Content[AElementID].Free;
  for i:= AElementID to High(Content)-1 do begin
    Content[AElementID]:= Content[AElementID+1];
  end;
	SetLength(Content, Length(Content)-1);
  Exit(true);
end;

procedure TFigures.Draw(APaintSpace: TPaintSpace);
var

  Figure: TFigure;
begin
  with APaintSpace.Canvas do begin
    for Figure in Content do begin
      Figure.Draw(APaintSpace);
    end;
  end;
end;

destructor TFigures.Destroy;
var
  Figure: TFigure;
begin
  for Figure in Content do
    Figure.Free;
end;

end.

