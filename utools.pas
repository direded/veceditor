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

	TShapeTool = class(TDrawingTool)
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

constructor TSelectTool.Create;
begin
  inherited Create;
  FMetadata.Name:= 'Select';
  FMetadata.Bitmap.LoadFromFile('src/select_tool.bmp');
end;

procedure TSelectTool.MouseDown(APoint: TDoublePoint; AShift: TShiftState);
begin
  FFirstPoint:= APoint;
  FSplitOff:= TRectSplitOffFigure.Create;
  FFigures.AddFigure(FSplitOff);
end;

procedure TSelectTool.MouseMove(APoint: TDoublePoint; AShift: TShiftState);
begin
  if (FFirstPoint - APoint).Length <= CLICK_SIZE then Exit;
  FSplitOff.Points[0]:= FFirstPoint;
  FSplitOff.Points[1]:= APoint;
end;

procedure TSelectTool.MouseUp(APoint: TDoublePoint; AShift: TShiftState);
begin
  FFigures.RemoveLastFigure;
  if (FFirstPoint - APoint).Length > CLICK_SIZE then Exit;
  if ssCtrl in AShift then
    Figures.ReverseSelectFigure(APoint)
  else if ssShift in AShift then
    FFigures.SelectFigure(APoint)
  else begin
    FFigures.UnSelectAllFigures;
    FFigures.SelectFigure(APoint);
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
  inherited Create;
  FParams:= TLineToolParameters.Create;
  FMetadata.Name:= 'Line';
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

procedure TShapeTool.CreateFigure;
begin
  FFigure:= TEllipseFigure.Create;
  Figures.AddFigure(FFigure);
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

constructor TShapeTool.Create;
begin
  inherited Create;
  FParams:= TShapeToolParameters.Create;
  FMetadata.Name:= 'Shape';
  FMetadata.Bitmap.LoadFromFile('src/shape_tool.bmp');
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
  inherited Create;
  FParams:= TRegularPolygonToolParameters.Create;
  FMetadata.Name:= 'RegularPolygon';
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
  inherited Create;
  FParams:= TRoundedRectToolParameters.Create;
  FMetadata.Name:= 'RoundedRect';
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
  RegisterTool(TShapeTool.Create);
  RegisterTool(TRegularPolygonTool.Create);
  RegisterTool(TRoundedRectTool.Create);
  RegisterTool(TZoomTool.Create);
end.

