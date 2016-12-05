unit UTools;

{$mode objfpc}{$H+}

interface
uses
	Classes, SysUtils, Controls, FileUtil, Forms, Graphics, Dialogs, Menus,
  ExtCtrls, Spin, StdCtrls, FPCanvas, UFigures, UDoublePoint, UPaintSpace, Math,
  UFigureParams, UUtils, UToolParams;

type

  TToolMetadata = record
    Name: String;
    Bitmap: TBitmap;
    Cursor: TCursor;
  end;

  StringArray = array of String;

  TParamChangingEvent = procedure(Sender: TObject) of object;
  TDrawItemEvent = procedure(Control: TWinControl; AIndex: Integer; ARect: TRect;
    AState: TOwnerDrawState) of object;

	TTool = class
  strict protected
    FParams: TToolParameters;
    FMetadata: TToolMetadata;
    FFigures: TFigures;
    FPaintSpace: TPaintSpace;
  public
    constructor Create;
    property Figures: TFigures read FFigures write FFigures;
    property Metadata: TToolMetadata read FMetadata;
    property PaintSpace: TPaintSpace write FPaintSpace;
    class procedure CleanParamsPanel(APanel: TWinControl);
    procedure SetParamColor(AFigureColors: TFigureColors); virtual;
    procedure SetParamsPanel(APanel: TPanel); virtual;
    procedure CleanUp; virtual;
    procedure MouseDown(APoint: TDoublePoint; AShift: TShiftState); virtual; abstract;
    procedure MouseMove(APoint: TDoublePoint; AShift: TShiftState); virtual; abstract;
  	procedure MouseUp(APoint:TDoublePoint; AShift: TShiftState); virtual; abstract;
  end;

  TZoomTool = class(TTool)
  strict private
    FFirstPoint: TDoublePoint;
    FSplitOff: TRectSplitOffFigure;
  public
    constructor Create;
    procedure SetParamsPanel(APanel: TPanel); override;
    procedure MouseDown(APoint: TDoublePoint; AShift: TShiftState); override;
    procedure MouseMove(APoint: TDoublePoint; AShift: TShiftState); override;
  	procedure MouseUp(APoint:TDoublePoint; AShift: TShiftState); override;
  end;

  THandTool = class(TTool)
  strict private
    FFirstPoint: TDoublePoint;
  public
    constructor Create;
    procedure MouseDown(APoint: TDoublePoint; AShift: TShiftState); override;
    procedure MouseMove(APoint: TDoublePoint; AShift: TShiftState); override;
  	procedure MouseUp(APoint:TDoublePoint; AShift: TShiftState); override;
  end;

	TSelectTool = class(TTool)
  private
    FFirstPoint: TDoublePoint;
    FSplitOff: TRectSplitOffFigure;
    const
      CLICK_SIZE: Integer = 3;
  public
  	constructor Create;
    procedure CleanUp; override;
    procedure MouseDown(APoint: TDoublePoint; AShift: TShiftState); override;
    procedure MouseMove(APoint: TDoublePoint; AShift: TShiftState); override;
  	procedure MouseUp(APoint:TDoublePoint; AShift: TShiftState); override;
  end;

  TDrawingTool = class(TTool)
  strict protected
    FFigure: TFigure;
    procedure Finish;
    procedure CreateFigure; virtual; abstract;
    procedure InitializeFigure(APoint: TDoublePoint); virtual; abstract;
    procedure SetFigureParams; virtual; abstract;
  public
  	procedure MouseUp(APoint:TDoublePoint; AShift: TShiftState); override;
  end;

	TLineTool = class(TDrawingTool)
  strict protected
    procedure CreateFigure; override;
    procedure InitializeFigure(APoint: TDoublePoint); override;
    procedure SetFigureParams; override;
  public
    constructor Create;
    procedure SetParamColor(AFigureColors: TFigureColors); override;
    procedure SetParamsPanel(APanel: TPanel); override;
    procedure MouseMove(APoint: TDoublePoint; AShift: TShiftState); override;
    procedure MouseDown(APoint: TDoublePoint; AShift: TShiftState); override;
	end;

  TPenTool = class(TLineTool)
  strict protected
    procedure InitializeFigure(APoint: TDoublePoint); override;
  public
    constructor Create;
    procedure MouseMove(APoint: TDoublePoint; AShift: TShiftState); override;
	end;

	TShapeTool = class(TDrawingTool)
  strict protected
    procedure InitializeFigure(APoint: TDoublePoint); override;
    procedure SetFigureParams; override;
  public
    procedure SetParamColor(AFigureColors: TFigureColors); override;
    procedure SetParamsPanel(APanel: TPanel); override;
    procedure MouseMove(APoint: TDoublePoint; AShift: TShiftState); override;
    procedure MouseDown(APoint: TDoublePoint; AShift: TShiftState); override;
  end;

  TEllipseTool = class(TShapeTool)
  strict protected
    procedure CreateFigure; override;
  public
    constructor Create;
  end;

  TRectTool = class(TShapeTool)
  strict protected
    procedure CreateFigure; override;
  public
    constructor Create;
  end;

  TRegularPolygonTool = class(TShapeTool)
  strict private
    FFirstPoint: TDoublePoint;
  strict protected
    procedure CreateFigure; override;
    procedure InitializeFigure(APoint: TDoublePoint); override;
    procedure SetFigureParams; override;
  public
    constructor Create;
    procedure SetParamsPanel(APanel: TPanel); override;
    procedure MouseMove(APoint: TDoublePoint; AShift: TShiftState); override;
    procedure MouseDown(APoint: TDoublePoint; AShift: TShiftState); override;
  end;

  TRoundedRectTool = class(TShapeTool)
  strict protected
    procedure CreateFigure; override;
    procedure InitializeFigure(APoint: TDoublePoint); override;
    procedure SetFigureParams; override;
  public
    constructor Create;
    procedure SetParamsPanel(APanel: TPanel); override;
    procedure MouseMove(APoint: TDoublePoint; AShift: TShiftState); override;
    procedure MouseDown(APoint: TDoublePoint; AShift: TShiftState); override;
  end;

	procedure RegisterTool(ATool: TTool);

var
	Tools: array of TTool;

implementation

procedure RegisterTool(ATool: TTool);
begin
	SetLength(Tools, Length(Tools)+1);
  Tools[High(Tools)]:= ATool;
end;

constructor TTool.Create;
begin
	FMetadata.Bitmap:= TBitmap.Create;
end;

class procedure TTool.CleanParamsPanel(APanel: TWinControl);
var
  i: Integer;
begin
  for i:= (APanel.ControlCount-1) downto 0 do
    APanel.Controls[i].Free;
end;

procedure TTool.SetParamColor(AFigureColors: TFigureColors);
begin

end;

procedure TTool.SetParamsPanel(APanel: TPanel);
begin

end;

procedure TTool.CleanUp;
begin

end;

constructor TSelectTool.Create;
begin
  inherited Create;
  FMetadata.Name:= 'Select';
  FMetadata.Bitmap.LoadFromFile('src/select_tool.bmp');
end;

procedure TSelectTool.CleanUp;
begin
  FFigures.UnSelectAllFigures;
  FPaintSpace.PaintBox.Invalidate;
end;

procedure TSelectTool.MouseDown(APoint: TDoublePoint; AShift: TShiftState);
begin
  FFirstPoint:= APoint;
  FSplitOff:= TRectSplitOffFigure.Create;
  FSplitOff.Points[0]:= APoint;
  FSplitOff.Points[1]:= APoint;
  FFigures.AddFigure(FSplitOff);
end;

procedure TSelectTool.MouseMove(APoint: TDoublePoint; AShift: TShiftState);
begin
  if (FFirstPoint - APoint).Length <= CLICK_SIZE then Exit;
  FSplitOff.Points[1]:= APoint;
end;

procedure TSelectTool.MouseUp(APoint: TDoublePoint; AShift: TShiftState);
begin
  FFigures.RemoveLastFigure;
  if (FFirstPoint-APoint).Length > CLICK_SIZE then begin
    if FFirstPoint.X<APoint.X then begin
      if not (ssShift in AShift) then
        Figures.UnSelectAllFigures;
      Figures.SpaceSelectFullFigures(FFirstPoint, APoint)
    end else begin
      if not (ssShift in AShift) then
        Figures.UnSelectAllFigures;
      Figures.SpaceSelectPartFigures(FFirstPoint, APoint);
      end;
  end else begin
    if ssCtrl in AShift then
      Figures.ReverseSelectFigure(APoint)
    else if ssShift in AShift then
      FFigures.SelectFigure(APoint)
    else begin
      FFigures.UnSelectAllFigures;
      FFigures.SelectFigure(APoint);
    end;
  end;
end;

constructor THandTool.Create;
begin
  inherited Create;
  FMetadata.Name:= 'Hand';
  FMetadata.Bitmap.LoadFromFile('src/hand_tool.bmp');
end;

procedure THandTool.MouseDown(APoint: TDoublePoint; AShift: TShiftState);
begin
  FFirstPoint:= APoint;
end;

procedure THandTool.MouseMove(APoint: TDoublePoint; AShift: TShiftState);
begin
  with FPaintSpace do begin
    Position:= Position-(APoint-FFirstPoint);
  end;
end;

procedure THandTool.MouseUp(APoint: TDoublePoint; AShift: TShiftState);
begin

end;

constructor TZoomTool.Create;
begin
  inherited Create;
  FParams:= TZoomToolParameters.Create;
  FMetadata.Name:= 'Zoom';
  FMetadata.Bitmap.LoadFromFile('src/zoom_tool.bmp');
  FFirstPoint:= GetDoublePoint;
end;

procedure TZoomTool.SetParamsPanel(APanel: TPanel);
begin
  FParams.FillUserInterface(APanel);
end;

procedure TZoomTool.MouseDown(APoint: TDoublePoint; AShift: TShiftState);
begin
  FFirstPoint:= APoint;
  if TZoomToolParameters(FParams).Mode = zmtlZoomSpace then begin
    FSplitOff:= TRectSplitOffFigure.Create;
    FSplitOff.SetPointsLength(2);
    FSplitOff.Points[0]:= APoint;
    FFigures.AddFigure(FSplitOff);
  end;
end;

procedure TZoomTool.MouseMove(APoint: TDoublePoint; AShift: TShiftState);
begin
  if TZoomToolParameters(FParams).Mode = zmtlZoomSpace then begin
    FSplitOff.Points[1]:= APoint;
  end;
end;

procedure TZoomTool.MouseUp(APoint: TDoublePoint; AShift: TShiftState);
begin
  if ssRight in AShift then exit;
  with TZoomToolParameters(FParams) do
  case Mode of
    zmtlZoomOut : FPaintSpace.SetScalePoint(FPaintSpace.Scale-ZoomPerClick, APoint);
    zmtlZoomIn : FPaintSpace.SetScalePoint(FPaintSpace.Scale+ZoomPerClick, APoint);
    zmtlZoomSpace : begin
      if abs(FFirstPoint.X-APoint.X) > abs(FFirstPoint.Y-APoint.Y) then
        FPaintSpace.Scale:= FPaintSpace.PaintBox.Width/abs(FFirstPoint.X-APoint.X)
      else
        FPaintSpace.Scale:= FPaintSpace.PaintBox.Height/abs(FFirstPoint.Y-APoint.Y);
      FPaintSpace.Position:= GetDoublePoint(min(FFirstPoint.X, APoint.X), min(FFirstPoint.Y, APoint.Y));
      FFigures.RemoveLastFigure;
    end;
  end;
end;

procedure TDrawingTool.Finish;
var
  Min, Max: TDoublePoint;
begin
  if FFigure.IsValid then begin
    FFigures.BakeLastFigure;
    FFigures.GetBounds(Min, Max);
    FPaintSpace.SetFiguresBounds(Min, Max);
  end else
    FFigures.RemoveLastFigure;
end;

procedure TDrawingTool.MouseUp(APoint: TDoublePoint; AShift: TShiftState);
begin
  Finish;
end;

constructor TLineTool.Create;
begin
  FParams:= TLineToolParameters.Create;
  FMetadata.Name:= 'Line';
  FMetadata.Bitmap:= TBitmap.Create;
  FMetadata.Bitmap.LoadFromFile('src/line_tool.bmp');
end;

procedure TLineTool.CreateFigure;
begin
  FFigure:= TLineFigure.Create;
  FFigures.AddFigure(FFigure);
end;

procedure TLineTool.InitializeFigure(APoint: TDoublePoint);
begin
  FFigure.Points[0]:= APoint;
  FFigure.Points[1]:= APoint;
end;

procedure TLineTool.SetFigureParams;
begin
  TLineFigure(FFigure).PenParams:= TLineToolParameters(FParams).Pen;
end;

procedure TLineTool.SetParamColor(AFigureColors: TFigureColors);
begin
  TLineToolParameters(FParams).Pen.Color:= AFigureColors.Pen;
end;

procedure TLineTool.SetParamsPanel(APanel: TPanel);
begin
  FParams.FillUserInterface(APanel);
end;

procedure TLineTool.MouseDown(APoint: TDoublePoint; AShift: TShiftState);
begin
  CreateFigure;
  InitializeFigure(APoint);
  SetFigureParams;
end;

procedure TLineTool.MouseMove(APoint: TDoublePoint; AShift: TShiftState);
begin
  FFigure.Points[1]:= APoint;
end;

procedure TPenTool.InitializeFigure(APoint: TDoublePoint);
begin
  FFigure.SetPointsLength(1);
  FFigure.Points[0]:= APoint;
end;

constructor TPenTool.Create;
begin
  FParams:= TLineToolParameters.Create;
  FMetadata.Name:= 'Pen';
  FMetadata.Bitmap:= TBitmap.Create;
  FMetadata.Bitmap.LoadFromFile('src/pen_tool.bmp');
end;

procedure TPenTool.MouseMove(APoint: TDoublePoint; AShift: TShiftState);
begin
  FFigure.IncreasePointsLength;
  FFigure.Points[High(FFigure.Points)]:= APoint;
end;

procedure TShapeTool.InitializeFigure(APoint: TDoublePoint);
begin
  FFigure.Points[0]:= APoint;
  FFigure.Points[1]:= APoint;
end;

procedure TShapeTool.SetFigureParams;
begin
  TShapeFigure(FFigure).PenParams:= TShapeToolParameters(FParams).Pen;
  TShapeFigure(FFigure).BrushParams:= TShapeToolParameters(FParams).Brush;
end;

procedure TShapeTool.SetParamColor(AFigureColors: TFigureColors);
begin
  TShapeToolParameters(FParams).Pen.Color:= AFigureColors.Pen;
  TShapeToolParameters(FParams).Brush.Color:= AFigureColors.Brush;
end;

procedure TShapeTool.SetParamsPanel(APanel: TPanel);
begin
  FParams.FillUserInterface(APanel);
end;

procedure TShapeTool.MouseDown(APoint: TDoublePoint; AShift: TShiftState);
begin
  CreateFigure;
  InitializeFigure(APoint);
  SetFigureParams;
end;

procedure TShapeTool.MouseMove(APoint: TDoublePoint; AShift: TShiftState);
begin
  FFigure.Points[1]:= APoint;
end;

procedure TEllipseTool.CreateFigure;
begin
  FFigure:= TEllipseFigure.Create;
  FFigures.AddFigure(FFigure);
end;

constructor TEllipseTool.Create;
begin
  FParams:= TShapeToolParameters.Create;
  FMetadata.Name:= 'Ellipse';
  FMetadata.Bitmap:= TBitmap.Create;
  FMetadata.Bitmap.LoadFromFile('src/ellipse_tool.bmp');
end;

procedure TRectTool.CreateFigure;
begin
  FFigure:= TRectFigure.Create;
  FFigures.AddFigure(FFigure);
end;

constructor TRectTool.Create;
begin
  FParams:= TShapeToolParameters.Create;
  FMetadata.Name:= 'Rect';
  FMetadata.Bitmap:= TBitmap.Create;
  FMetadata.Bitmap.LoadFromFile('src/rect_tool.bmp');
end;

procedure TRegularPolygonTool.CreateFigure;
begin
  FFigure:= TPolygonFigure.Create;
  FFigure.SetPointsLength(TRegularPolygonToolParameters(FParams).AngleCount);
  FFigures.AddFigure(FFigure);
end;

procedure TRegularPolygonTool.InitializeFigure(APoint: TDoublePoint);
begin
  inherited InitializeFigure(APoint);
end;

procedure TRegularPolygonTool.SetFigureParams;
begin
  inherited SetFigureParams;
end;

constructor TRegularPolygonTool.Create;
begin
  FParams:= TRegularPolygonToolParameters.Create;
  FMetadata.Name:= 'RegularPolygon';
  FMetadata.Bitmap:= TBitmap.Create;
  FMetadata.Bitmap.LoadFromFile('src/regular_tool.bmp');
  TRegularPolygonToolParameters(FParams).AngleCount:= 3;
end;

procedure TRegularPolygonTool.SetParamsPanel(APanel: TPanel);
begin
  FParams.FillUserInterface(APanel);
end;

procedure TRegularPolygonTool.MouseDown(APoint: TDoublePoint; AShift: TShiftState);
begin
  CreateFigure;
  InitializeFigure(APoint);
  SetFigureParams;
  FFirstPoint:= APoint;
end;

procedure TRegularPolygonTool.MouseMove(APoint: TDoublePoint; AShift: TShiftState);
var
  i: Integer;
  vec: TDoublePoint;
begin
  vec:= APoint - FFirstPoint;
  with TRegularPolygonToolParameters(FParams) do
    for i:= 0 to AngleCount-1 do begin
      FFigure.Points[i]:= vec+FFirstPoint;
      vec.Rotate(2*pi/AngleCount);
    end;
end;

procedure TRoundedRectTool.CreateFigure;
begin
  FFigure:= TRoundedRectFigure.Create;
  FFigures.AddFigure(FFigure);
end;

procedure TRoundedRectTool.InitializeFigure(APoint: TDoublePoint);
begin
  inherited InitializeFigure(APoint);
end;

procedure TRoundedRectTool.SetFigureParams;
begin
  inherited SetFigureParams;
  TRoundedRectFigure(FFigure).Rounding:= TRoundedRectToolParameters(FParams).Rounding;
end;

constructor TRoundedRectTool.Create;
begin
  FParams:= TRoundedRectToolParameters.Create;
  FMetadata.Name:= 'RoundedRect';
  FMetadata.Bitmap:= TBitmap.Create;
  FMetadata.Bitmap.LoadFromFile('src/rect_tool.bmp');
end;

procedure TRoundedRectTool.SetParamsPanel(APanel: TPanel);
begin
  FParams.FillUserInterface(APanel);
end;

procedure TRoundedRectTool.MouseDown(APoint: TDoublePoint; AShift: TShiftState);
begin
  CreateFigure;
  InitializeFigure(APoint);
  SetFigureParams;
end;

procedure TRoundedRectTool.MouseMove(APoint: TDoublePoint; AShift: TShiftState);
begin
  FFigure.Points[1]:= APoint;
end;

initialization
  RegisterTool(TSelectTool.Create);
  RegisterTool(THandTool.Create);
  RegisterTool(TLineTool.Create);
  RegisterTool(TPenTool.Create);
  RegisterTool(TRectTool.Create);
  RegisterTool(TEllipseTool.Create);
  RegisterTool(TRegularPolygonTool.Create);
  RegisterTool(TRoundedRectTool.Create);
  RegisterTool(TZoomTool.Create);
end.

