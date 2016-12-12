unit UTools;

{$mode objfpc}{$H+}

interface
uses
	Classes, Types, Controls, Graphics,
  ExtCtrls, StdCtrls, UFigures, UDoublePoint, UPaintSpace, Math,
  UFigureParams, UToolParams;

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
    FParams: TToolParamArray;
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
    procedure SetParamsPanel(APanel: TPanel);
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
    FIsFirstOnFigure: Boolean;
    FLastPoint: TDoublePoint;
    procedure FRevers(AFigure: TFigure);
    procedure FSelect(AFigure: TFigure);
    procedure FSelectAllBtnClick(Sender: TObject);
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
  public
    constructor Create;
    procedure MouseMove(APoint: TDoublePoint; AShift: TShiftState); override;
    procedure MouseDown(APoint: TDoublePoint; AShift: TShiftState); override;
  end;

  TRoundedRectTool = class(TShapeTool)
  strict protected
    procedure CreateFigure; override;
    procedure SetFigureParams; override;
  public
    constructor Create;
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
var
  i: Integer;
begin
  for i:= 0 to High(FParams) do
    FParams[i].FillUserInterface(APanel);
end;

procedure TTool.CleanUp;
begin

end;

procedure TSelectTool.FRevers(AFigure: TFigure);
begin
  AFigure.Selected:= not AFigure.Selected;
end;

procedure TSelectTool.FSelect(AFigure: TFigure);
begin
  AFigure.Selected:= true;
end;

procedure TSelectTool.FSelectAllBtnClick(Sender: TObject);
begin
  FFigures.SelectAllFigures;
  FPaintSpace.PaintBox.Invalidate;
end;

constructor TSelectTool.Create;
begin
  inherited Create;
  FMetadata.Name:= 'Select';
  SetLength(FParams, 1);
  FParams[0]:= ToolParams[0];
  TSelectToolParam(FParams[0]).SelectAllBtnClick:= @FSelectAllBtnClick;
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
  FLastPoint:= APoint;
  FSplitOff:= TRectSplitOffFigure.Create;
  FSplitOff.Points[0]:= APoint;
  FSplitOff.Points[1]:= APoint;
  FFigures.AddFigure(FSplitOff);
  FIsFirstOnFigure:= false;
  if FFigures.GetFigure(FFirstPoint) <> nil then
    FIsFirstOnFigure:= FFigures.GetFigure(FFirstPoint).Selected;
end;

procedure TSelectTool.MouseMove(APoint: TDoublePoint; AShift: TShiftState);
begin
  if FIsFirstOnFigure then begin
      FFigures.MoveSelected(APoint-FLastPoint);
      FLastPoint:= APoint;
      Exit;
  end;
  if ((FFirstPoint - APoint).Length <= CLICK_SIZE) then Exit;
  FSplitOff.Points[1]:= APoint;
end;

procedure TSelectTool.MouseUp(APoint: TDoublePoint; AShift: TShiftState);
begin
  FFigures.RemoveLastFigure;
  if FIsFirstOnFigure then Exit;
  if (FFirstPoint-APoint).Length > CLICK_SIZE then begin
    if FFirstPoint.X<APoint.X then begin
      if ssShift in AShift then
        FFigures.SetSelectionFullRectFigures(FFirstPoint, APoint, @FSelect)
      else if ssCtrl in AShift then
        FFigures.SetSelectionFullRectFigures(FFirstPoint, APoint, @FRevers)
      else begin
        FFigures.UnSelectAllFigures;
        FFigures.SetSelectionFullRectFigures(FFirstPoint, APoint, @FSelect)
      end;
    end else begin
      if ssShift in AShift then
        FFigures.SetSelectionPartRectFigures(FFirstPoint, APoint, @FSelect)
      else if ssCtrl in AShift then
        FFigures.SetSelectionPartRectFigures(FFirstPoint, APoint, @FRevers)
      else begin
        FFigures.UnSelectAllFigures;
        FFigures.SetSelectionPartRectFigures(FFirstPoint, APoint, @FSelect)
      end;
    end;
  end
  else begin
    if ssShift in AShift then
      FFigures.SetSelectionFigure(APoint, @FSelect)
    else if ssCtrl in AShift then
      FFigures.SetSelectionFigure(APoint, @FRevers)
    else begin
      FFigures.UnSelectAllFigures;
      FFigures.SetSelectionFigure(APoint, @FSelect);
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
  SetLength(FParams, 1);
  FParams[0]:= ToolParams[1];
  FMetadata.Name:= 'Zoom';
  FMetadata.Bitmap.LoadFromFile('src/zoom_tool.bmp');
  FFirstPoint:= GetDoublePoint;
end;

procedure TZoomTool.MouseDown(APoint: TDoublePoint; AShift: TShiftState);
begin
  FFirstPoint:= APoint;
  if TZoomToolParam(FParams[0]).Mode = zmtlZoomSpace then begin
    FSplitOff:= TRectSplitOffFigure.Create;
    FSplitOff.SetPointsLength(2);
    FSplitOff.Points[0]:= APoint;
    FFigures.AddFigure(FSplitOff);
  end;
end;

procedure TZoomTool.MouseMove(APoint: TDoublePoint; AShift: TShiftState);
begin
  if TZoomToolParam(FParams).Mode = zmtlZoomSpace then begin
    FSplitOff.Points[1]:= APoint;
  end;
end;

procedure TZoomTool.MouseUp(APoint: TDoublePoint; AShift: TShiftState);
begin
  if ssRight in AShift then exit;
  with TZoomToolParam(FParams) do
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
  SetLength(FParams, 1);
  FParams[0]:= ToolParams[2];
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
  TLineFigure(FFigure).PenParams:= TLineToolParam(FParams[0]).Pen;
end;

procedure TLineTool.SetParamColor(AFigureColors: TFigureColors);
begin
  TLineToolParam(FParams[0]).Pen.Color:= AFigureColors.Pen;
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
  SetLength(FParams, 1);
  FParams[0]:= ToolParams[2];
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
  TShapeFigure(FFigure).PenParams:= TLineToolParam(FParams[0]).Pen;
  TShapeFigure(FFigure).BrushParams:= TShapeToolParam(FParams[1]).Brush;
end;

procedure TShapeTool.SetParamColor(AFigureColors: TFigureColors);
begin
  TLineToolParam(FParams[0]).Pen.Color:= AFigureColors.Pen;
  TShapeToolParam(FParams[1]).Brush.Color:= AFigureColors.Brush;
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
  SetLength(FParams, 2);
  FParams[0]:= ToolParams[2];
  FParams[1]:= ToolParams[3];
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
  SetLength(FParams, 2);
  FParams[0]:= ToolParams[2];
  FParams[1]:= ToolParams[3];
  FMetadata.Name:= 'Rect';
  FMetadata.Bitmap:= TBitmap.Create;
  FMetadata.Bitmap.LoadFromFile('src/rect_tool.bmp');
end;

procedure TRegularPolygonTool.CreateFigure;
begin
  FFigure:= TPolygonFigure.Create;
  FFigure.SetPointsLength(TRegularPolygonToolParam(FParams[2]).AngleCount);
  FFigures.AddFigure(FFigure);
end;

constructor TRegularPolygonTool.Create;
begin
  FMetadata.Name:= 'RegularPolygon';
  SetLength(FParams, 3);
  FParams[0]:= ToolParams[2];
  FParams[1]:= ToolParams[3];
  FParams[2]:= ToolParams[4];
  FMetadata.Bitmap:= TBitmap.Create;
  FMetadata.Bitmap.LoadFromFile('src/regular_tool.bmp');
  TRegularPolygonToolParam(FParams[2]).AngleCount:= 3;
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
  with TRegularPolygonToolParam(FParams[2]) do
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

procedure TRoundedRectTool.SetFigureParams;
begin
  inherited SetFigureParams;
  TRoundedRectFigure(FFigure).Rounding:= TRoundedRectToolParam(FParams[2]).Rounding;
end;

constructor TRoundedRectTool.Create;
begin
  SetLength(FParams, 3);
  FParams[0]:= ToolParams[2];
  FParams[1]:= ToolParams[3];
  FParams[2]:= ToolParams[5];
  FMetadata.Name:= 'RoundedRect';
  FMetadata.Bitmap:= TBitmap.Create;
  FMetadata.Bitmap.LoadFromFile('src/rect_tool.bmp');
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

