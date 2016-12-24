unit UFigures;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, Controls, FileUtil, Forms, Graphics, Dialogs, Menus,
  ExtCtrls, Math, UDoublePoint, UPaintSpace, FPCanvas, UUtils, UFigureParams,
  UGeometry;

type

  TTwoDoublePointsArray = array[0..1] of TDoublePoint;

  TFigure = class
  strict protected
    FBounds: TTwoDoublePointsArray;
    FPoints: TDoublePointArray;
    FSelected: Boolean;
    procedure SetFigureParams(ACanvas: TCanvas); virtual; abstract;
    procedure DrawRaw(APaintSpace: TPaintSpace); virtual; abstract;
    class procedure SetSelectionEllipseParams(APen: TPen; ABrush: TBrush);
    procedure DrawSelection(APaintSpace: TPaintSpace); virtual; abstract;
    procedure SetSelectionParams(ACanvas:TCanvas); virtual; abstract;
    procedure LoadParams(var AParamStr: String); virtual; //abstract;
    procedure LoadPoints(var AParamStr: String);
    procedure CopyParamsTo(AFigure: TFigure); virtual;
  public
    property Points: TDoublePointArray read FPoints write FPoints;
    property Bounds: TTwoDoublePointsArray read FBounds write FBounds;
    property Selected: Boolean read FSelected write FSelected;
    constructor Create;
    function IsPointInclude(APoint: TDoublePoint): Boolean; virtual; abstract;
    function IsFullInRect(A, B: TDoublePoint): Boolean; virtual; abstract;
    function IsPartInRect(A, B: TDoublePoint): Boolean; virtual; abstract;
    function IsValid: Boolean; virtual; abstract;
    procedure Move(AValue: TDoublePoint);
    procedure SetPointsLength(ALength: Integer);
    procedure IncreasePointsLength;
    procedure Bake; virtual;
    function GetParams: TFigureParamArray; virtual; abstract;
    procedure Save(var AFile: TextFile); virtual;
    procedure Draw(APaintSpace: TPaintSpace); virtual;
    procedure Load(AParamStr: String);
    function Copy: TFigure; virtual;
  end;

  TSelectionChangeProcedure = procedure(AFigure: TFigure) of object;
  TFigureClass = class of TFigure;
  TFigureClassArray = array of TFigureClass;
  TFigureArray = array of TFigure;

  TLineFigure = class(TFigure)
  strict protected
    FLineColor: TColorParam;
    FLineWidth: TLineWidthParam;
    FLineStyle: TLineStyleParam;
    function GetLineParams: TPenParams;
    procedure SetLineParams(AValue: TPenParams);
    procedure SetSelectionParams(ACanvas: TCanvas); override;
    procedure DrawSelection(APaintSpace: TPaintSpace); override;
    procedure SetFigureParams(ACanvas: TCanvas); override;
    procedure DrawRaw(APaintSpace: TPaintSpace); override;
    procedure LoadParams(var AParamStr: String); override;
    procedure CopyParamsTo(AFigure: TFigure); override;
  public
    property PenParams: TPenParams read GetLineParams;
    constructor Create;
    property LineColor: TColorParam read FLineColor;
    property LineWidth: TLineWidthParam read FLineWidth;
    property LineStyle: TLineStyleParam read FLineStyle;
    function GetParams: TFigureParamArray; override;
    function IsPointInclude(APoint: TDoublePoint): Boolean; override;
    function IsFullInRect(A, B: TDoublePoint): Boolean; override;
    function IsPartInRect(A, B: TDoublePoint): Boolean; override;
    function IsValid: Boolean; override;
    procedure Save(var AFile: TextFile); override;
    function Copy: TFigure; override;
  end;

  TShapeFigure = class(TLineFigure)
  strict protected
    FShapeColor: TColorParam;
    FShapeStyle: TShapeStyleParam;
    procedure SetSelectionParams(ACanvas: TCanvas); override;
    procedure SetFigureParams(ACanvas: TCanvas); override;
    function GetBrushParams: TBrushParams;
    procedure SetBrushParams(AValue: TBrushParams);
    procedure LoadParams(var AParamStr: String); override;
    procedure CopyParamsTo(AFigure: TFigure); override;
  public
    constructor Create;
    property ShapeColor: TColorParam read FShapeColor;
    property ShapeStyle: TShapeStyleParam read FShapeStyle;
    function IsPartInRect(A, B: TDoublePoint): Boolean; override;
    property BrushParams: TBrushParams read GetBrushParams;
    function GetParams: TFigureParamArray; override;
    function IsValid: Boolean; override;
    procedure Save(var AFile: TextFile); override;
  end;

  TRectFigure = class(TShapeFigure)
  public
    constructor Create;
    procedure DrawRaw(APaintSpace: TPaintSpace); override;
    function IsPointInclude(APoint: TDoublePoint): Boolean; override;
    function Copy: TFigure; override;
  end;

  TRegularPolygonFigure = class(TShapeFigure)
  strict protected
    FAngleCountParam: TAngleCountParam;
    procedure DrawSelection(APaintSpace: TPaintSpace); override;
    procedure DrawRaw(APaintSpace: TPaintSpace); override;
    function GetAngleCount: Integer;
    procedure SetAngleCount(AValue: Integer);
    procedure LoadParams(var AParamStr: String); override;
    procedure CopyParamsTo(AFigure: TFigure); override;
  public
    property AngleCount: Integer read GetAngleCount;
    property AngleCountParam: TAngleCountParam read FAngleCountParam;
    constructor Create;
    function IsValid: Boolean; override;
    function GetParams: TFigureParamArray; override;
    function IsPointInclude(APoint: TDoublePoint): Boolean; override;
    procedure Save(var AFile: TextFile); override;
    function Copy: TFigure; override;
  end;

  TRectSplitOffFigure = class(TRectFigure)
  public
    constructor Create;
    function IsPointInclude(APoint: TDoublePoint): Boolean; override;
    procedure Draw(APaintSpace: TPaintSpace); override;
  end;

  TEllipseFigure = class(TShapeFigure)
  strict protected
    procedure DrawSelection(APaintSpace: TPaintSpace); override;
    procedure DrawRaw(APaintSpace: TPaintSpace); override;
	public
    constructor Create;
    function IsPointInclude(APoint: TDoublePoint): Boolean; override;
    function Copy: TFigure; override;
  end;

  TRoundedRectFigure = class(TShapeFigure)
  strict private
    FRoundingParam: TRoundingParam;
    function GetRounding: Integer;
    procedure SetRounding(AValue: Integer);
  strict protected
    procedure DrawSelection(APaintSpace: TPaintSpace); override;
    procedure DrawRaw(APaintSpace: TPaintSpace); override;
    procedure LoadParams(var AParamStr: String); override;
    procedure CopyParamsTo(AFigure: TFigure); override;
	public
    property Rounding: Integer read GetRounding write SetRounding;
    property RoundingParam: TRoundingParam read FRoundingParam;
    constructor Create;
    function IsPointInclude(APoint: TDoublePoint): Boolean; override;
    function GetParams: TFigureParamArray; override;
    procedure Save(var AFile: TextFile); override;
    function Copy: TFigure; override;
  end;

  TFigures = class
 	strict private
    FContent: TFigureArray;
    FFigureAddEvent: TEventHandler;
    FBounds: TDoubleRect;
    function GetFigure(AIndex: Integer): TFigure;
    procedure SetFigure(AIndex: Integer; AFigure: TFigure);
    procedure SetBounds;
    function CompareFigureClass(AClassName: String): TFigureClass;
    function CreateFigure(AClassName: String): TFigure;
  public
    property OnFigureAdd: TEventHandler read FFigureAddEvent write FFigureAddEvent;
    constructor Create;
    property Figure[AIndex: Integer]: TFigure read GetFigure write SetFigure; default;
    property Content: TFigureArray read FContent write FContent;
    procedure SetSelectionFigure(APoint: TDoublePoint; AChangeSelection: TSelectionChangeProcedure);
    procedure SetSelectionFullRectFigures(A, B: TDoublePoint; AChangeSelection: TSelectionChangeProcedure);
    procedure SetSelectionPartRectFigures(A, B: TDoublePoint; AChangeSelection: TSelectionChangeProcedure);
    function GetFigure(APoint: TDoublePoint): TFigure;
    function GetSelectedFigures: TFigureArray;
    function GetFullRectFigures(A, B: TDoublePoint): TFigureArray;
    function GetPartRectFigures(A, B: TDoublePoint): TFigureArray;
    procedure SelectAllFigures;
    procedure UnSelectAllFigures;
    function CanSelectFigure(APoint: TDoublePoint): Boolean;
    procedure MoveSelected(AValue: TDoublePoint);
    procedure AddFigure(AFigure: TFigure);
    function RemoveFigure(AElementID: Longint): Boolean;
    function RemoveLastFigure: Boolean;
    procedure BakeLastFigure;
    procedure GetBounds(var AMin: TDoublePoint; var AMax: TDoublePoint);
  	procedure Draw(APaintSpace: TPaintSpace);
    procedure Save(var AFile: TextFile);
    procedure Load(var AFile: TextFile);
    function CopyContent: TFigureArray;
    procedure SetContent(AValue: TFigureArray);
    destructor Destroy; override;
  end;

  procedure SetBounds(var ABounds: TTwoDoublePointsArray; APoint: TDoublePoint);
  function GetFigureClasses: TFigureClassArray;

  function CopyOf(Arr: TFigureArray): TFigureArray; overload;

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

function GetFigureClasses: TFigureClassArray;
begin
  SetLength(Result, 5);
  Result[0]:= TLineFigure;
  Result[1]:= TRectFigure;
  Result[2]:= TEllipseFigure;
  Result[3]:= TRegularPolygonFigure;
  Result[4]:= TRoundedRectFigure;
end;

function CopyOf(Arr: TFigureArray): TFigureArray;
var
  i: Integer;
begin
  SetLength(Result, Length(Arr));
  for i:= Low(Arr) to High(Arr) do begin
    Result[i]:= Arr[i].Copy;
  end;
end;

class procedure TFigure.SetSelectionEllipseParams(APen: TPen; ABrush: TBrush);
begin
  APen.Color:= RGBToColor(100, 100, 100);
  APen.Width:= 1;
  APen.Style:= psSolid;
  ABrush.Style:= bsClear;
  ABrush.Color:= clWhite;
end;

procedure TFigure.LoadParams(var AParamStr: String);
begin

end;

procedure TFigure.LoadPoints(var AParamStr: String);
var
  i: Integer;
  str, pointsStr: String;
begin
  AParamStr:= Trim(AParamStr);
  str:= Trim(LeftStr(AParamStr, Pos(' ', AParamStr)-1));
  pointsStr:= Trim(RightStr(AParamStr, Length(AParamStr)-Length(str)-1));
  SetLength(FPoints, StrToInt(str));
  for i:= 0 to High(FPoints) do begin
    str:= Trim(LeftStr(pointsStr, Pos(' ', pointsStr)-1));
    pointsStr:= Trim(RightStr(pointsStr, Length(pointsStr)-Length(str)-1));
    FPoints[i].X:= StrToFloat(str);
    str:= Trim(LeftStr(pointsStr, Pos(' ', pointsStr)-1));
    pointsStr:= Trim(RightStr(pointsStr, Length(pointsStr)-Length(str)-1));
    FPoints[i].Y:= StrToFloat(str);
  end;
  AParamStr:= Trim(pointsStr)+' ';
end;

procedure TFigure.CopyParamsTo(AFigure: TFigure);
begin
  AFigure.Points:= CopyOf(FPoints);
end;

procedure TFigure.IncreasePointsLength;
begin
  SetLength(FPoints, Length(FPoints)+1);
end;

constructor TFigure.Create;
begin
end;

procedure TFigure.Move(AValue: TDoublePoint);
var
  i: Integer;
begin
  for i:= 0 to High(FPoints) do
    FPoints[i]+= AValue;
  FBounds[0]+= AValue;
  FBounds[1]+= AValue;
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

procedure TFigure.Save(var AFile: TextFile);
var
  p: TDoublePoint;
begin
  Write(AFile, Length(FPoints), ' ');
  for p in FPoints do begin
    Write(AFile, p.X, ' ');
    Write(AFile, p.Y, ' ');
  end;
end;

procedure TFigure.Draw(APaintSpace: TPaintSpace);
begin
  with APaintSpace do begin
    SetFigureParams(Canvas);
    if FSelected then
      SetSelectionParams(Canvas);
    DrawRaw(APaintSpace);
    if FSelected then
      DrawSelection(APaintSpace);
  end;
end;

procedure TFigure.Load(AParamStr: String);
begin
  LoadPoints(AParamStr);
  LoadParams(AParamStr);
end;

function TFigure.Copy: TFigure;
begin
  //Result:= TFigure(Self.ClassType.Create);
  //Result:= TLineFigure.Create;
  //Result.Points:= CopyOf(FPoints);
end;

procedure TLineFigure.DrawSelection(APaintSpace: TPaintSpace);
begin
  with APaintSpace do begin
    SetSelectionEllipseParams(Canvas.Pen, Canvas.Brush);
    Canvas.EllipseC(ToLocal(FPoints[0]).X, ToLocal(FPoints[0]).Y, 3, 3);
    Canvas.EllipseC(ToLocal(FPoints[High(FPoints)]).X, ToLocal(FPoints[High(FPoints)]).Y, 3, 3);
  end;
end;

function TLineFigure.GetLineParams: TPenParams;
begin
  Result.Color:= FLineColor.Value;
  Result.Style:= FLineStyle.Value;
  Result.Width:= FLineWidth.Value;
end;

procedure TLineFigure.SetLineParams(AValue: TPenParams);
begin
  FLineWidth.Value:= AValue.Width;
  FLineStyle.Value:= AValue.Style;
  FLineColor.Value:= AValue.Color;
end;

procedure TLineFigure.SetSelectionParams(ACanvas: TCanvas);
begin
  ACanvas.Pen.Width:= ACanvas.Pen.Width+1;
end;

procedure TLineFigure.SetFigureParams(ACanvas: TCanvas);
begin
  SetCanvasParams(PenParams, ACanvas.Pen);
end;

procedure TLineFigure.DrawRaw(APaintSpace: TPaintSpace);
begin
  APaintSpace.Canvas.Polyline(APaintSpace.ToLocal(FPoints));
end;

constructor TLineFigure.Create;
begin
  SetLength(FPoints, 2);
  FLineWidth:= TLineWidthParam.Create;
  FLineStyle:= TLineStyleParam.Create;
  FLineColor:= TColorParam.Create;
end;

function TLineFigure.GetParams: TFigureParamArray;
begin
  SetLength(Result, Length(Result)+3);
  Result[High(Result)-2]:= FLineColor;
  Result[High(Result)-1]:= FLineStyle;
  Result[High(Result)]:= FLineWidth;
end;

function TLineFigure.IsPointInclude(APoint: TDoublePoint): Boolean;
var
  i: Integer;
  A, B: TDoublePoint;
begin
  for i:= 0 to High(FPoints)-1 do begin
    A:= FPoints[i]; B:= FPoints[i+1];
    if IsPointInLineSegment(A, B, APoint, 1600) then Exit(true);
  end;
  Exit(False);
end;

function TLineFigure.IsFullInRect(A, B: TDoublePoint): Boolean;
var
  p: TDoublePoint;
begin
  for p in FPoints do
    if not (InRange(p.X, Min(A.X, B.X), Max(A.X, B.X)) and
      InRange(p.Y, Min(A.Y, B.Y), Max(A.Y, B.Y))) then
      Exit(false);
  Exit(true);
end;

function TLineFigure.IsPartInRect(A, B: TDoublePoint): Boolean; // FIX!
var
  p: TDoublePoint;
begin
  for p in FPoints do
    if InRange(p.X, Min(A.X, B.X), Max(A.X, B.X)) and
      InRange(p.Y, Min(A.Y, B.Y), Max(A.Y, B.Y)) then
      Exit(true);
  Exit(false);
end;

function TLineFigure.IsValid: Boolean;
begin
  Result:= (Length(FPoints)>=2);
end;

procedure TLineFigure.Save(var AFile: TextFile);
begin
  inherited Save(AFile);
  Write(AFile, FLineColor.Value, ' ');
  Write(AFile, Integer(FLineStyle.Value), ' ');
  Write(AFile, FLineWidth.Value, ' ');
end;

function TLineFigure.Copy: TFigure;
begin
  Result:= TLineFigure.Create;
  CopyParamsTo(Result);
end;

procedure TLineFigure.LoadParams(var AParamStr: String);
var
  str, otherStr: String;
begin
  str:= LeftStr(AParamStr, Pos(' ', AParamStr)-1);
  otherStr:= RightStr(AParamStr, Length(AParamStr)-Length(str)-1);
  FLineColor.Value:= StringToColor(str);
  str:= LeftStr(otherStr, Pos(' ', otherStr)-1);
  otherStr:= RightStr(otherStr, Length(otherStr)-Length(str)-1);
  FLineStyle.Value:= TFPPenStyle(StrToInt(str));
  str:= LeftStr(otherStr, Pos(' ', otherStr)-1);
  otherStr:= RightStr(otherStr, Length(otherStr)-Length(str)-1);
  FLineWidth.Value:= StrToInt(str);
  AParamStr:= otherStr;
end;

procedure TLineFigure.CopyParamsTo(AFigure: TFigure);
begin
  inherited CopyParamsTo(AFigure);
  TLineFigure(AFigure).LineStyle.Value:= FLineStyle.Value;
  TLineFigure(AFigure).LineWidth.Value:= FLineWidth.Value;
  TLineFigure(AFigure).LineColor.Value:= FLineColor.Value;
  AFigure.Bounds:= FBounds;
end;

procedure TShapeFigure.Save(var AFile: TextFile);
begin
  inherited Save(AFile);
  Write(AFile, FShapeColor.Value, ' ');
  Write(AFile, Integer(FShapeStyle.Value), ' ');
end;

procedure TShapeFigure.SetSelectionParams(ACanvas: TCanvas);
begin
  ACanvas.Pen.Width:= ACanvas.Pen.Width+1;
  ACanvas.Brush.Color:= ColorToRGB(ACanvas.Brush.Color)-$101010;
end;

procedure TShapeFigure.SetFigureParams(ACanvas: TCanvas);
begin
  SetCanvasParams(PenParams, ACanvas.Pen);
  SetCanvasParams(BrushParams, ACanvas.Brush);
end;

function TShapeFigure.GetBrushParams: TBrushParams;
begin
  Result.Color:= FShapeColor.Value;
  Result.Style:= FShapeStyle.Value;
end;

procedure TShapeFigure.SetBrushParams(AValue: TBrushParams);
begin
  FShapeColor.Value:= AValue.Color;
  FShapeStyle.Value:= AValue.Style;
end;

procedure TShapeFigure.LoadParams(var AParamStr: String);
var
  str, otherStr: String;
begin
  inherited LoadParams(AParamStr);
  otherStr:= AParamStr;
  str:= LeftStr(otherStr, Pos(' ', otherStr)-1);
  otherStr:= RightStr(otherStr, Length(otherStr)-Length(str)-1);
  FShapeColor.Value:= StringToColor(str);
  str:= LeftStr(otherStr, Pos(' ', otherStr)-1);
  otherStr:= RightStr(otherStr, Length(otherStr)-Length(str)-1);
  FShapeStyle.Value:= TFPBrushStyle(StrToInt(str));
  AParamStr:= otherStr;
end;

procedure TShapeFigure.CopyParamsTo(AFigure: TFigure);
begin
  inherited CopyParamsTo(AFigure);
  TShapeFigure(AFigure).ShapeStyle.Value:= FShapeStyle.Value;
  TShapeFigure(AFigure).ShapeColor.Value:= FShapeColor.Value;
end;

function TShapeFigure.IsPartInRect(A, B: TDoublePoint): Boolean;
var
  p: TDoublePoint;
begin
  for p in FBounds do
    if InRange(p.X, Min(A.X, B.X), Max(A.X, B.X)) and
      InRange(p.Y, Min(A.Y, B.Y), Max(A.Y, B.Y)) then
      Exit(true);
  Exit(false);
end;

constructor TShapeFigure.Create;
begin
  inherited Create;
  FShapeColor:= TColorParam.Create;
  FShapeStyle:= TShapeStyleParam.Create;
end;

function TShapeFigure.GetParams: TFigureParamArray;
begin
  Result:= inherited GetParams;
  SetLength(Result, Length(Result)+2);
  Result[High(Result)-1]:= FShapeColor;
  Result[High(Result)]:= FShapeStyle;
end;

function TShapeFigure.IsValid: Boolean;
begin
  Result:= (FPoints[0].X <> FPoints[1].X) and (FPoints[0].Y <> FPoints[1].Y);
end;

constructor TRectFigure.Create;
begin
  inherited Create;
end;

procedure TRectFigure.DrawRaw(APaintSpace: TPaintSpace);
begin
  with APaintSpace do
    Canvas.Rectangle(ToLocal(FPoints[0]).X, ToLocal(FPoints[0]).Y,
                     ToLocal(FPoints[1]).X, ToLocal(FPoints[1]).Y);
end;

function TRectFigure.IsPointInclude(APoint: TDoublePoint): Boolean;
begin
  if (FBounds[0].X<=APoint.X) and (APoint.X<=FBounds[1].X) and
    (FBounds[0].Y<=APoint.Y) and (APoint.Y<=FBounds[1].Y) then
    Exit(true);
  Exit(false);
end;

function TRectFigure.Copy: TFigure;
begin
  Result:= TRectFigure.Create;
  CopyParamsTo(Result);
end;

procedure TRegularPolygonFigure.DrawSelection(APaintSpace: TPaintSpace);
var
  p: TDoublePoint;
begin
  with APaintSpace do begin
    SetSelectionEllipseParams(Canvas.Pen, Canvas.Brush);
    for p in FPoints do
      Canvas.EllipseC(ToLocal(p).X, ToLocal(p).Y, 3, 3);
  end;
end;

procedure TRegularPolygonFigure.DrawRaw(APaintSpace: TPaintSpace);
var
  vec: TDoublePoint;
  ps: TDoublePointArray;
  i: Integer;
begin
  vec:= FPoints[1]-FPoints[0];
  SetLength(ps, AngleCount);
  for i:= 0 to AngleCount-1 do begin
    ps[i]:= vec+FPoints[0];
    vec.Rotate(2*pi/AngleCount);
  end;
  APaintSpace.Canvas.Polygon(APaintSpace.ToLocal(ps));
end;

function TRegularPolygonFigure.GetAngleCount: Integer;
begin
  Result:= FAngleCountParam.Value;
end;

procedure TRegularPolygonFigure.SetAngleCount(AValue: Integer);
begin
  FAngleCountParam.Value:= AValue;
end;

procedure TRegularPolygonFigure.LoadParams(var AParamStr: String);
var
  str, otherStr: String;
begin
  inherited LoadParams(AParamStr);
  otherStr:= AParamStr;
  str:= LeftStr(otherStr, Pos(' ', otherStr)-1);
  otherStr:= RightStr(otherStr, Length(otherStr)-Length(str)-1);
  FAngleCountParam.Value:= StrToInt(str);
  AParamStr:= otherStr;
end;

procedure TRegularPolygonFigure.CopyParamsTo(AFigure: TFigure);
begin
  inherited CopyParamsTo(AFigure);
  TRegularPolygonFigure(AFigure).AngleCountParam.Value:= FAngleCountParam.Value;
end;

constructor TRegularPolygonFigure.Create;
begin
  inherited;
  FAngleCountParam:= TAngleCountParam.Create;
end;

function TRegularPolygonFigure.IsValid: Boolean;
begin
  Result:= FPoints[0] <> FPoints[1];
end;

function TRegularPolygonFigure.GetParams: TFigureParamArray;
begin
  Result:= inherited GetParams;
  SetLength(Result, Length(Result)+1);
  Result[High(Result)]:= FAngleCountParam;
end;

function TRegularPolygonFigure.IsPointInclude(APoint: TDoublePoint): Boolean;
var
  vec: TDoublePoint;
  ps: TDoublePointArray;
  i: Integer;
begin
  vec:= FPoints[1]-FPoints[0];
  SetLength(ps, AngleCount);
  for i:= 0 to AngleCount-1 do begin
    ps[i]:= vec+FPoints[0];
    vec.Rotate(2*pi/AngleCount);
  end;
  for i:= 0 to High(ps)-1 do
    if GetVecMultiplyLength(ps[i+1]-ps[i], APoint-ps[i]) < 0 then
      Exit(false);
  if GetVecMultiplyLength(ps[0]-ps[High(ps)], APoint-ps[High(ps)]) < 0 then
    Exit(false);
  Result:= true;
end;

procedure TRegularPolygonFigure.Save(var AFile: TextFile);
begin
  inherited Save(AFile);
  Write(AFile, FAngleCountParam.Value, ' ');
end;

function TRegularPolygonFigure.Copy: TFigure;
begin
  Result:= TRegularPolygonFigure.Create;
  CopyParamsTo(Result);
end;

constructor TRectSplitOffFigure.Create;
begin
  inherited Create;
  FLineStyle.Value:= psDash;
end;

function TRectSplitOffFigure.IsPointInclude(APoint: TDoublePoint): Boolean;
begin

end;

procedure TRectSplitOffFigure.Draw(APaintSpace: TPaintSpace);
begin
  with APaintSpace do begin
    SetCanvasParams(PenParams, Canvas.Pen);
    Canvas.Frame(ToLocal(FPoints[0]).X, ToLocal(FPoints[0]).Y,
                 ToLocal(FPoints[1]).X, ToLocal(FPoints[1]).Y);
  end;
end;

procedure TEllipseFigure.DrawSelection(APaintSpace: TPaintSpace);
var
  middle: TDoublePoint;
begin
  with APaintSpace do begin
    middle:= GetDoublePoint(0.5*FBounds[0].X+0.5*FBounds[1].X,
                            0.5*FBounds[0].Y+0.5*FBounds[1].Y);
    SetSelectionEllipseParams(Canvas.Pen, Canvas.Brush);
    Canvas.EllipseC(ToLocal(middle).X, ToLocal(middle).Y, 3, 3);
    Canvas.EllipseC(ToLocal(middle).X, ToLocal(FBounds[0]).Y, 3, 3);
    Canvas.EllipseC(ToLocal(middle).X, ToLocal(FBounds[1]).Y, 3, 3);
    Canvas.EllipseC(ToLocal(FBounds[0]).X, ToLocal(middle).Y, 3, 3);
    Canvas.EllipseC(ToLocal(FBounds[1]).X, ToLocal(middle).Y, 3, 3);
  end;
end;

procedure TEllipseFigure.DrawRaw(APaintSpace: TPaintSpace);
begin
  with APaintSpace do
    Canvas.Ellipse(ToLocal(FPoints[0]).X, ToLocal(FPoints[0]).Y,
                   ToLocal(FPoints[1]).X, ToLocal(FPoints[1]).Y);
end;

constructor TEllipseFigure.Create;
begin
  inherited Create;
end;

function TEllipseFigure.IsPointInclude(APoint: TDoublePoint): Boolean;
var
  F1, F2, L, R, O: TDoublePoint;
  c: Double;
begin
  L:= FBounds[0];
  R:= FBounds[1];
  O:= L+(R-L)/2;
  APoint:= APoint-O; L:= L-O; R:= R-O;
  if R.X-L.X > R.Y-L.Y then begin
    c:= sqrt(sqr(R.X)-sqr(L.Y));
    F1:= GetDoublePoint(-c ,0); F2:= GetDoublePoint(c, 0);
    if (APoint-F1).Length+(APoint-F2).Length <= 2*R.X then Exit(true);
  end else begin
    c:= sqrt(sqr(L.Y)-sqr(R.X));
    F1:= GetDoublePoint(0, c); F2:= GetDoublePoint(0, -c);
    if (APoint-F1).Length+(APoint-F2).Length <= -2*L.Y then Exit(true);
  end;
  Exit(false);
end;

function TEllipseFigure.Copy: TFigure;
begin
  Result:= TEllipseFigure.Create;
  CopyParamsTo(Result);
end;

function TRoundedRectFigure.GetRounding: Integer;
begin
  Result:= FRoundingParam.Value;
end;

procedure TRoundedRectFigure.SetRounding(AValue: Integer);
begin
  FRoundingParam.Value:= AValue;
end;

procedure TRoundedRectFigure.DrawSelection(APaintSpace: TPaintSpace);
begin
  inherited;
end;

procedure TRoundedRectFigure.DrawRaw(APaintSpace: TPaintSpace);
begin
  with APaintSpace do
    Canvas.RoundRect(ToLocal(FPoints[0]).X, ToLocal(FPoints[0]).Y,
                     ToLocal(FPoints[1]).X, ToLocal(FPoints[1]).Y, Rounding, Rounding);
end;

procedure TRoundedRectFigure.LoadParams(var AParamStr: String);
var
  str, otherStr: String;
begin
  inherited LoadParams(AParamStr);
  otherStr:= AParamStr;
  str:= LeftStr(otherStr, Pos(' ', otherStr)-1);
  otherStr:= RightStr(otherStr, Length(otherStr)-Length(str)-1);
  FRoundingParam.Value:= StrToInt(str);
  AParamStr:= otherStr;
end;

procedure TRoundedRectFigure.CopyParamsTo(AFigure: TFigure);
begin
  inherited CopyParamsTo(AFigure);
  TRoundedRectFigure(AFigure).RoundingParam.Value:= FRoundingParam.Value;
end;

constructor TRoundedRectFigure.Create;
begin
  inherited Create;
  FRoundingParam:= TRoundingParam.Create;
end;

function TRoundedRectFigure.IsPointInclude(APoint: TDoublePoint): Boolean;
begin
  Result :=
    InRange(APoint.X, FBounds[0].X, FBounds[1].X) and
    InRange(APoint.Y, FBounds[0].Y, FBounds[1].Y);
end;

function TRoundedRectFigure.GetParams: TFigureParamArray;
begin
  Result:= inherited GetParams;
  SetLength(Result, Length(Result)+1);
  Result[High(Result)]:= FRoundingParam;
end;

procedure TRoundedRectFigure.Save(var AFile: TextFile);
begin
  inherited Save(AFile);
  Write(AFile, FRoundingParam.Value, ' ');
end;

function TRoundedRectFigure.Copy: TFigure;
begin
  Result:= TRoundedRectFigure.Create;
  CopyParamsTo(Result);
end;

constructor TFigures.Create;
begin
	FContent:= nil
end;

procedure TFigures.SetSelectionFigure(APoint: TDoublePoint; AChangeSelection: TSelectionChangeProcedure);
var
  i: Integer;
begin
  for i:= High(FContent) downto 0 do
    if FContent[i].IsPointInclude(APoint) then begin
      AChangeSelection(FContent[i]);
      Exit;
    end;
end;

procedure TFigures.SetSelectionFullRectFigures(A, B: TDoublePoint; AChangeSelection: TSelectionChangeProcedure);
var
  i: Integer;
begin
  for i:= High(FContent) downto 0 do
    if FContent[i].IsFullInRect(A, B) then
      AChangeSelection(FContent[i]);
end;

procedure TFigures.SetSelectionPartRectFigures(A, B: TDoublePoint; AChangeSelection: TSelectionChangeProcedure);
var
  i: Integer;
begin
  for i:= High(FContent) downto 0 do
    if FContent[i].IsPartInRect(A, B) then
      AChangeSelection(FContent[i]);
end;

function TFigures.GetFigure(APoint: TDoublePoint): TFigure;
var
  i: Integer;
begin
for i:= High(FContent) downto 0 do
    if FContent[i].IsPointInclude(APoint) then begin
      Exit(FContent[i]);
    end;
  Result:= nil;
end;

function TFigures.GetSelectedFigures: TFigureArray;
var
  f: TFigure;
begin
  for f in FContent do
    if f.Selected then begin
      SetLength(Result, Length(Result)+1);
      Result[High(Result)]:= f;
    end;
end;

function TFigures.GetFullRectFigures(A, B: TDoublePoint): TFigureArray;      // COMMON CODE (#01)
var
  f: TFigure;
begin
  for f in FContent do
    if f.IsFullInRect(A, B) then begin
      SetLength(Result, Length(Result)+1);
      Result[High(Result)]:= f;
    end;
end;

function TFigures.GetPartRectFigures(A, B: TDoublePoint): TFigureArray;      // COMMON CODE (#01)
var
  f: TFigure;
begin
  for f in FContent do
    if f.IsPartInRect(A, B) then begin
      SetLength(Result, Length(Result)+1);
      Result[High(Result)]:= f;
    end;
end;

procedure TFigures.SelectAllFigures;
var
  f: TFigure;
begin
  for f in FContent do
    f.Selected:= true;
end;

procedure TFigures.UnSelectAllFigures;
var
  f: TFigure;
begin
  for f in FContent do
    f.Selected:= false;
end;

function TFigures.CanSelectFigure(APoint: TDoublePoint): Boolean;
var
    i: Integer;
begin
  for i:= High(FContent) downto 0 do
    if FContent[i].IsPointInclude(APoint) then
      Exit(true);
  Result:= false;
end;

procedure TFigures.MoveSelected(AValue: TDoublePoint);
var
  f: TFigure;
begin
  for f in FContent do
    if f.Selected then f.Move(AValue);
end;

procedure TFigures.AddFigure(AFigure: TFigure);
begin
  SetLength(FContent, Length(FContent)+1);
  FContent[High(FContent)]:= AFigure;
end;


function TFigures.GetFigure(AIndex: Integer): TFigure;
begin
  Result:= Content[AIndex];
end;

procedure TFigures.SetFigure(AIndex: Integer; AFigure: TFigure);
begin
  Content[AIndex]:= AFigure;
end;

procedure TFigures.SetBounds;
var
  f: TFigure;
  Min, Max: TDoublePoint;
begin
  if FContent = nil then exit;
  Min:= FContent[0].Bounds[0];
  Max:= FContent[0].Bounds[1];
  for f in FContent do begin
    if Min.X > f.Bounds[0].X then
      Min.X:= f.Bounds[0].X;
    if Min.Y > f.Bounds[0].Y then
      Min.Y:= f.Bounds[0].Y;
    if Max.X < f.Bounds[1].X then
      Max.X:= f.Bounds[1].X;
    if Max.Y < f.Bounds[1].Y then
      Max.Y:= f.Bounds[1].Y;
  end;
  Min.X:= Min.X-5; Min.Y:= Min.Y-5;
  Max.X:= Max.X+5; Max.Y:= Max.Y+5;
  FBounds.TopLeft:= Min;
  FBounds.Width:= Max.X-FBounds.TopLeft.X;
  FBounds.Height:= Max.Y-FBounds.TopLeft.Y;
end;

function TFigures.CompareFigureClass(AClassName: String): TFigureClass;
var
  fc: TFigureClass;
begin
  for fc in GetFigureClasses do begin
    if fc.ClassName = AClassName then Exit(fc);
  end;
  Result:= nil;
end;

function TFigures.CreateFigure(AClassName: String): TFigure;
{var
  fcs: TFigureClassArray;
  f: TFigure;
  i: Integer;}
begin
  //fcs:= GetFigureClasses;     (?) WTF (?)
  {for i:= 0 to High(fcs) do begin
    f:= TLineFigure.Create;//fcs[i].Create;
    if f.ClassName = AClassName then Exit(f)
    else f.Free;
  end;}
  case AClassName of
    'TLineFigure' : Exit(TLineFigure.Create);
    'TRectFigure' : Exit(TRectFigure.Create);
    'TEllipseFigure' : Exit(TEllipseFigure.Create);
    'TRegularPolygonFigure' : Exit(TRegularPolygonFigure.Create);
    'TRoundedRectFigure' : Exit(TRoundedRectFigure.Create);
  end;
  Result:= nil;
end;

function TFigures.RemoveLastFigure: Boolean;
begin
  if(Length(FContent) = 0) then Exit(false);
  FContent[High(FContent)].Free;
  SetLength(FContent, Length(FContent)-1);
end;

procedure TFigures.BakeLastFigure;
begin
  FContent[High(FContent)].Bake;
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
  if(AElementID < Low(FContent)) or (AElementID >= High(FContent)) then Exit(false);
	FContent[AElementID].Free;
  for i:= AElementID to High(FContent)-1 do begin
    FContent[AElementID]:= FContent[AElementID+1];
  end;
	SetLength(FContent, Length(FContent)-1);
  Exit(true);
end;

procedure TFigures.Draw(APaintSpace: TPaintSpace);
var
  i: Integer;
begin
  for i:= 0 to High(FContent) do
    FContent[i].Draw(APaintSpace);
end;

procedure TFigures.Save(var AFile: TextFile);
var
  f: TFigure;
begin
  WriteLn(AFile, '## ', Length(Content), ' ');
  for f in Content do begin
    Write(AFile, '# '+f.ClassName+' ');
    f.Save(AFile);
    Writeln(AFile, '');
  end;
end;

procedure TFigures.Load(var AFile: TextFile);
var
  f: TFigure;
  str: String;
  i: Integer;
  fClassName, fParamsStr: String;
begin
  for f in FContent do
    f.Free;
  FContent:= nil;

  while Pos('##', str) = 0 do begin
    Readln(AFile, str);
  end;
  str:= RightStr(str, Length(str)-Pos('##', str)-2);
  SetLength(FContent, StrToInt(Trim(str)));

  for i:= 0 to High(FContent) do begin
    while Pos('#', str) = 0 do
      Readln(AFile, str);
    str:= RightStr(str, Length(str)-Pos('#', str)-1);
    str:= Trim(str);
    fClassName:= LeftStr(str, Pos(' ', str)-1);
    fParamsStr:= RightStr(str, Length(str)-Pos(' ', str));
    FContent[i]:= CreateFigure(fClassName);
    FContent[i].Load(fParamsStr);
  end;
  if Assigned(FFigureAddEvent) then
    FFigureAddEvent;
end;

function TFigures.CopyContent: TFigureArray;
begin
  Result:= CopyOf(FContent);
end;

procedure TFigures.SetContent(AValue: TFigureArray);
var
  f: TFigure;
begin
  for f in FContent do
    f.Free;
  SetLength(FContent, 0);
  FContent:= CopyOf(AValue);
  if Assigned(FFigureAddEvent) then
    FFigureAddEvent;
end;

destructor TFigures.Destroy;
var
  f: TFigure;
begin
  for f in FContent do
    f.Free;
end;

end.
