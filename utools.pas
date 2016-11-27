unit UTools;

{$mode objfpc}{$H+}

interface
uses
	Classes, SysUtils, Controls, FileUtil, Forms, Graphics, Dialogs, Menus,
  ExtCtrls, Spin, StdCtrls, FPCanvas, UFigures, UDoublePoint, UPaintSpace, Math,
  UFigureParams, UUtils;

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
    FMetadata: TToolMetadata;
    FFigures: TFigures;
    FPaintSpace: TPaintSpace;
    function CreateParamComboBox(AOwner: TWinControl; AItems: array of String; AStartValue: Integer;
      AEvent: TParamChangingEvent): TComboBox;
    function CreateParamComboBox(AOwner: TWinControl; AItemCount: Integer; AStartValue: Integer; ADrawItemEvent: TDrawItemEvent;
      AChangingEvent: TParamChangingEvent): TComboBox;
    function CreateParamSpinEdit(AOwner: TWinControl; AMin, AMax: Integer; AStartValue: Integer;
      AEvent: TParamChangingEvent): TSpinEdit;
    function CreateParamFloatSpinEdit(AOwner: TWinControl; AMin, AMax: Double; AStartValue: Double;
      AEvent: TParamChangingEvent): TFloatSpinEdit;
  public
    constructor Create;
    procedure SetParamColor(AFigureColors: TFigureColors); virtual;
    property Figures: TFigures read FFigures write FFigures;
    property Metadata: TToolMetadata read FMetadata;
    property PaintSpace: TPaintSpace write FPaintSpace;
    procedure SetParamsPanel(APanel: TPanel); virtual;
    procedure MouseDown(APoint: TDoublePoint; AShift: TShiftState); virtual; abstract;
    procedure MouseMove(APoint: TDoublePoint; AShift: TShiftState); virtual; abstract;
  	procedure MouseUp(APoint:TDoublePoint; AShift: TShiftState); virtual; abstract;
  end;

  TZoomTool = class(TTool)
  strict private
    FParamMode: (zmtlZoomIn, zmtlZoomOut, zmtlZoomSpace);
    FParamZoomPerClick: Double;
    FFirstPoint: TDoublePoint;
    FSplitOff: TRectSplitOffFigure;
    const FParamModeDisplayString: array[0..2] of String = ('Zoom in', 'Zoom out', 'Zoom space');
    procedure ParamModeChange(Sender: TObject);
    procedure ParamZoomPerClickChange(Sender: TObject);
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
  public
  	constructor Create;
    procedure MouseDown(APoint: TDoublePoint; AShift: TShiftState); override;
    procedure MouseMove(APoint: TDoublePoint; AShift: TShiftState); override;
  	procedure MouseUp(APoint:TDoublePoint; AShift: TShiftState); override;
  end;

  TDrawingTool = class(TTool)
  strict protected
    FFigure: TFigure;
  public
  	procedure MouseUp(APoint:TDoublePoint; AShift: TShiftState); override;
  end;

	TLineTool = class(TDrawingTool)
  strict private
    FParamFigure: (lntlLine, lntlPen);
    FPenParams: TPenParams;
    const
      FParamFigureDisplayString: array[0..1] of String = ('Line', 'Pen');
    procedure ParamPenStyleComboBoxDrawItem(Control: TWinControl; AIndex: Integer; ARect: TRect;
      AState: TOwnerDrawState);
    procedure ParamFigureChange(Sender: TObject);
    procedure ParamPenWidthChange(Sender: TObject);
    procedure ParamPenStyleChange(Sender: TObject);
  public
    constructor Create;
    procedure SetParamColor(AFigureColors: TFigureColors); override;
    procedure SetParamsPanel(APanel: TPanel); override;
    procedure MouseMove(APoint: TDoublePoint; AShift: TShiftState); override;
    procedure MouseDown(APoint: TDoublePoint; AShift: TShiftState); override;
	end;

	TShapeTool = class(TDrawingTool)
  strict private                // WTF????? STRICT PRIVATE
    FParamFigure: (shfigEllipse, shfigRect);
  strict protected
    FPenParams: TPenParams;
    FBrushParams: TBrushParams;
    const
      FParamFigureDisplayString: array[0..1] of String = ('Ellipse', 'Rect');
    procedure ParamPenStyleComboBoxDrawItem(Control: TWinControl; AIndex: Integer; ARect: TRect;
      AState: TOwnerDrawState);
    procedure ParamBrushStyleComboBoxDrawItem(Control: TWinControl; AIndex: Integer; ARect: TRect;
          AState: TOwnerDrawState);
    procedure ParamFigureChange(Sender: TObject);
    procedure ParamPenWidthChange(Sender: TObject);
    procedure ParamPenStyleChange(Sender: TObject);
    procedure ParamBrushStyleChange(Sender: TObject);
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
    FParamAngleCount: Integer;
    procedure ParamAngleCountChange(Sender: TObject);
  public
    constructor Create;
    procedure SetParamsPanel(APanel: TPanel); override;
    procedure MouseMove(APoint: TDoublePoint; AShift: TShiftState); override;
    procedure MouseDown(APoint: TDoublePoint; AShift: TShiftState); override;
  end;

  TRoundedRectTool = class(TShapeTool)
  strict private
    FFirstPoint: TDoublePoint;
    FParamRounding: Integer;
    procedure ParamRoundingChange(Sender: TObject);
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

function TTool.CreateParamComboBox(AOwner: TWinControl; AItemCount: Integer; AStartValue: Integer; ADrawItemEvent: TDrawItemEvent;
  AChangingEvent: TParamChangingEvent): TComboBox;
var i:Integer;
begin
  Result:= TComboBox.Create(AOwner);
  Result.Width:= 80;
  Result.ReadOnly:= true;
  for i:= 0 to AItemCount-1 do
    Result.Items.Add('');
  Result.ItemIndex:= AStartValue;
  Result.Style:= csOwnerDrawFixed;
  Result.OnChange:= AChangingEvent;
  Result.OnDrawItem:= ADrawItemEvent;
end;

function TTool.CreateParamComboBox(AOwner: TWinControl; AItems: array of String; AStartValue: Integer;
  AEvent: TParamChangingEvent): TComboBox;
var
  str: String;
begin
  if Length(AItems) = 0 then Exit;
  Result:= TComboBox.Create(AOwner);
  Result.Width:= 80;
  Result.ReadOnly:= true;
  for str in AItems do
    Result.Items.Add(str);
  Result.ItemIndex:= AStartValue;
  Result.OnChange:= AEvent;
end;

function TTool.CreateParamFloatSpinEdit(AOwner: TWinControl; AMin, AMax: Double; AStartValue: Double;
  AEvent: TParamChangingEvent): TFloatSpinEdit;
begin
  Result:= TFloatSpinEdit.Create(AOwner);
  Result.Width:= 85;
  Result.OnChange:= AEvent;
  Result.Value:= AStartValue;
  Result.MaxValue:= AMax;
  Result.MinValue:= AMin;
end;

function TTool.CreateParamSpinEdit(AOwner: TWinControl; AMin, AMax: Integer; AStartValue: Integer;
  AEvent: TParamChangingEvent): TSpinEdit;
begin
  Result:= TSpinEdit.Create(AOwner);
  Result.Width:= 50;
  Result.OnChange:= AEvent;
  Result.Value:= AStartValue;
  Result.MaxValue:= AMax;
  Result.MinValue:= AMin;
end;

constructor TTool.Create;
begin
	FMetadata.Bitmap:= TBitmap.Create;
end;

procedure TTool.SetParamColor(AFigureColors: TFigureColors);
begin

end;

procedure TTool.SetParamsPanel(APanel: TPanel);
var i: Integer;
begin
  for i:= (APanel.ControlCount-1) downto 0 do
    APanel.Controls[i].Free;
end;

constructor TSelectTool.Create;
begin
  inherited Create;
  FMetadata.Name:= 'Select';
  FMetadata.Bitmap.LoadFromFile('src/select_tool.bmp');
end;

procedure TSelectTool.MouseDown(APoint: TDoublePoint; AShift: TShiftState);
begin
end;

procedure TSelectTool.MouseMove(APoint: TDoublePoint; AShift: TShiftState);
begin
end;

procedure TSelectTool.MouseUp(APoint: TDoublePoint; AShift: TShiftState);
begin
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

procedure TZoomTool.ParamModeChange(Sender: TObject);
begin
  case TComboBox(Sender).ItemIndex of
    0 : FParamMode:= zmtlZoomIn;
    1 : FParamMode:= zmtlZoomOut;
    2 : FParamMode:= zmtlZoomSpace;
  end;
end;

procedure TZoomTool.ParamZoomPerClickChange(Sender: TObject);
begin
  FParamZoomPerClick:= TFloatSpinEdit(Sender).Value;
end;

constructor TZoomTool.Create;
begin
  inherited Create;
  FMetadata.Name:= 'Zoom';
  FMetadata.Bitmap.LoadFromFile('src/zoom_tool.bmp');
  FFirstPoint:= GetDoublePoint;
  FParamMode:= zmtlZoomIn;
  FParamZoomPerClick:= 0.25;
end;

procedure TZoomTool.SetParamsPanel(APanel: TPanel);
var
  TotalLeft: Integer;
  Param: TWinControl;
const
  Left = 5;
  Top = 8;
begin
  inherited SetParamsPanel(APanel);

  Param:= CreateParamFloatSpinEdit(APanel, 0.10, 2, FParamZoomPerClick, @ParamZoomPerClickChange);
  Param.Parent:= APanel;
  Param.Left:= Left;
  Param.Top:= Top;
  TotalLeft:= Param.Width+Param.Left+Left;

  Param:= CreateParamComboBox(APanel, FParamModeDisplayString, Integer(FParamMode), @ParamModeChange);
  Param.Parent:= APanel;
  Param.Left:= TotalLeft;
  Param.Top:= Top;
end;

procedure TZoomTool.MouseDown(APoint: TDoublePoint; AShift: TShiftState);
begin
  FFirstPoint:= APoint;
  if FParamMode = zmtlZoomSpace then begin
    FSplitOff:= TRectSplitOffFigure.Create;
    FSplitOff.SetPointsLength(2);
    FSplitOff.Points[0]:= APoint;
    FFigures.AddFigure(FSplitOff);
  end;
end;

procedure TZoomTool.MouseMove(APoint: TDoublePoint; AShift: TShiftState);
begin
  if FParamMode = zmtlZoomSpace then begin
    FSplitOff.Points[1]:= APoint;
  end;
end;

procedure TZoomTool.MouseUp(APoint: TDoublePoint; AShift: TShiftState);
begin
  if ssRight in AShift then exit;
  case FParamMode of
    zmtlZoomOut : FPaintSpace.SetScalePoint(FPaintSpace.Scale-FParamZoomPerClick, APoint);
    zmtlZoomIn : FPaintSpace.SetScalePoint(FPaintSpace.Scale+FParamZoomPerClick, APoint);
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

procedure TDrawingTool.MouseUp(APoint: TDoublePoint; AShift: TShiftState);
var
  Min, Max: TDoublePoint;
begin
  if FFigure.IsValid then begin
    FFigures.BakeLastFigure;
    FFigures.GetBounds(Min, Max);
    FPaintSpace.SetFiguresBounds(Min, Max);
  end
  else
    FFigures.RemoveLastFigure;
end;

constructor TLineTool.Create;
begin
  inherited Create;
  FMetadata.Name:= 'Line';
  FMetadata.Bitmap.LoadFromFile('src/line_tool.bmp');
  FPenParams.Width:= 1;
  FParamFigure:= lntlLine;
end;

procedure TLineTool.ParamFigureChange(Sender: TObject);
begin
  case TComboBox(Sender).ItemIndex of
    0 : FParamFigure:= lntlLine;
    1 : FParamFigure:= lntlPen;
  end;
end;

procedure TLineTool.ParamPenStyleComboBoxDrawItem(Control: TWinControl; AIndex: Integer; ARect: TRect;
  AState: TOwnerDrawState);
begin
  case AIndex of
    0 : TComboBox(Control).Canvas.Pen.Style:= psSolid;
    1 : TComboBox(Control).Canvas.Pen.Style:= psDash;
    2 : TComboBox(Control).Canvas.Pen.Style:= psDot;
    3 : TComboBox(Control).Canvas.Pen.Style:= psDashDot;
    4 : TComboBox(Control).Canvas.Pen.Style:= psDashDotDot;
  end;
  TComboBox(Control).Canvas.Line(ARect.Left+7, ARect.Bottom div 2+1, ARect.Right-7, ARect.Bottom div 2+1);
end;

procedure TLineTool.ParamPenWidthChange(Sender: TObject);
begin
  FPenParams.Width:= TSpinEdit(Sender).Value;
end;

procedure TLineTool.ParamPenStyleChange(Sender: TObject);
begin
  FPenParams.Style:= TFPPenStyle(TComboBox(Sender).ItemIndex);
end;

procedure TLineTool.SetParamColor(AFigureColors: TFigureColors);
begin
  FPenParams.Color:= AFigureColors.Pen;
end;

procedure TLineTool.SetParamsPanel(APanel: TPanel);
var
  TotalLeft: Integer;
  Param: TWinControl;
const
  Left: Integer = 5;
  Top: Integer = 8;
begin
  inherited SetParamsPanel(APanel);

  Param:= CreateParamComboBox(APanel, FParamFigureDisplayString, Integer(FParamFigure), @ParamFigureChange);
  Param.Parent:= APanel;
  Param.Left:= Left;
  Param.Top:= Top;
  TotalLeft:= Param.Width + Param.Left + Left;

  Param:= CreateParamSpinEdit(APanel, 1, 100, FPenParams.Width, @ParamPenWidthChange);
  Param.Parent:= APanel;
  Param.Left:= TotalLeft;
  Param.Top:= Top;
  TotalLeft:= Param.Width + Param.Left + Left;

  Param:= CreateParamComboBox(APanel, 8, Integer(FPenParams.Style), @ParamPenStyleComboBoxDrawItem,
    @ParamPenStyleChange);
  Param.Parent:= APanel;
  Param.Left:= TotalLeft;
  Param.Top:= Top;
end;

procedure TLineTool.MouseDown(APoint: TDoublePoint; AShift: TShiftState);
begin
  FFigure:= TLineFigure.Create;
  if FParamFigure = lntlLine then begin
    FFigure.SetPointsLength(2);
    FFigure.Points[1]:= APoint
  end
  else
    FFigure.SetPointsLength(1);
  TLineFigure(FFigure).PenParams:= FPenParams;
  FFigure.Points[0]:= APoint;
  FFigures.AddFigure(FFigure);
end;

procedure TLineTool.MouseMove(APoint: TDoublePoint; AShift: TShiftState);
begin
  case FParamFigure of
    lntlPen : begin
      FFigure.IncreasePointsLength;
      FFigure.Points[High(FFigure.Points)]:= APoint;
    end;

    lntlLine : begin
      FFigure.Points[1]:= APoint;
    end
  end;
end;

procedure TShapeTool.ParamPenStyleComboBoxDrawItem(Control: TWinControl; AIndex: Integer; ARect: TRect;
  AState: TOwnerDrawState);
begin
  case AIndex of
    0 : TComboBox(Control).Canvas.Pen.Style:= psSolid;
    1 : TComboBox(Control).Canvas.Pen.Style:= psDash;
    2 : TComboBox(Control).Canvas.Pen.Style:= psDot;
    3 : TComboBox(Control).Canvas.Pen.Style:= psDashDot;
    4 : TComboBox(Control).Canvas.Pen.Style:= psDashDotDot;
  end;
  TComboBox(Control).Canvas.Line(ARect.Left+7, ARect.Bottom div 2+1, ARect.Right-7, ARect.Bottom div 2+1);
end;

procedure TShapeTool.ParamBrushStyleComboBoxDrawItem(Control: TWinControl; AIndex: Integer; ARect: TRect;
  AState: TOwnerDrawState);
var
  rect: TRect;
begin
  rect.Left:= ARect.Left+8;
  rect.Right:= ARect.Right-8;
  rect.Top:= ARect.Top+4;
  rect.Bottom:= ARect.Bottom-3;
  case AIndex of
    0 : TComboBox(Control).Canvas.Brush.Style:= bsSolid;
    1 : TComboBox(Control).Canvas.Brush.Style:= bsClear;
    2 : TComboBox(Control).Canvas.Brush.Style:= bsHorizontal;
    3 : TComboBox(Control).Canvas.Brush.Style:= bsVertical;
    4 : TComboBox(Control).Canvas.Brush.Style:= bsFDiagonal;
    5 : TComboBox(Control).Canvas.Brush.Style:= bsBDiagonal;
    6 : TComboBox(Control).Canvas.Brush.Style:= bsCross;
    7 : TComboBox(Control).Canvas.Brush.Style:= bsDiagCross;
  end;
  TComboBox(Control).Canvas.Pen.Color:= clGray;
  TComboBox(Control).Canvas.Brush.Color:= clGray;
  TComboBox(Control).Canvas.Rectangle(rect);
end;

procedure TShapeTool.ParamFigureChange(Sender: TObject);
begin
  case TComboBox(Sender).ItemIndex of
    0 : FParamFigure:= shfigEllipse;
    1 : FParamFigure:= shfigRect;
  end;
end;

procedure TShapeTool.ParamPenWidthChange(Sender: TObject);
begin
  FPenParams.Width:= TSpinEdit(Sender).Value;
end;

procedure TShapeTool.ParamPenStyleChange(Sender: TObject);
begin
  FPenParams.Style:= TFPPenStyle(TComboBox(Sender).ItemIndex);
end;

procedure TShapeTool.ParamBrushStyleChange(Sender: TObject);
begin
  FBrushParams.Style:= TFPBrushStyle(TComboBox(Sender).ItemIndex);
end;

constructor TShapeTool.Create;
begin
  inherited Create;
  FMetadata.Name:= 'Rect';
  FMetadata.Bitmap.LoadFromFile('src/rect_tool.bmp');
end;

procedure TShapeTool.SetParamColor(AFigureColors: TFigureColors);
begin
  FPenParams.Color:= AFigureColors.Pen;
  FBrushParams.Color:= AFigureColors.Brush;
end;

procedure TShapeTool.SetParamsPanel(APanel: TPanel);
var
  Param: TWinControl;
  TotalLeft: Integer;
const
  Left: Integer = 5;
  Top: Integer = 8;
begin
  inherited SetParamsPanel(APanel);
  Param:= CreateParamComboBox(APanel, FParamFigureDisplayString, Integer(FParamFigure), @ParamFigureChange);
  Param.Parent:= APanel;
  Param.Left:= Left;
  Param.Top:= Top;
  TotalLeft:= Param.Left+Param.Width+Left;

  Param:= CreateParamSpinEdit(APanel, 1, 100, FPenParams.Width, @ParamPenWidthChange);
  Param.Parent:= APanel;
  Param.Left:= TotalLeft;
  Param.Top:= Top;
  TotalLeft:= Param.Left+Param.Width+Left;

  Param:= CreateParamComboBox(APanel, 4, Integer(FPenParams.Style), @ParamPenStyleComboBoxDrawItem, @ParamPenStyleChange);
  Param.Parent:= APanel;
  Param.Left:= TotalLeft;
  Param.Top:= Top;
  TotalLeft:= Param.Left+Param.Width+Left;

  Param:= CreateParamComboBox(APanel, 8, Integer(FBrushParams.Style), @ParamBrushStyleComboBoxDrawItem, @ParamBrushStyleChange);
  Param.Parent:= APanel;
  Param.Left:= TotalLeft;
  Param.Top:= Top;
  TotalLeft:= Param.Left+Param.Width+Left;
end;

procedure TShapeTool.MouseDown(APoint: TDoublePoint; AShift: TShiftState);
begin
  case FParamFigure of
    shfigEllipse : FFigure:= TEllipseFigure.Create;
    shfigRect : FFigure:= TRectFigure.Create else
      FFigure:= TPolygonFigure.Create;
  end;
  FFigure.SetPointsLength(2);
  FFigure.Points[0]:= APoint;
  FFigure.Points[1]:= APoint;
  TShapeFigure(FFigure).PenParams:= FPenParams;
  TShapeFigure(FFigure).BrushParams:= FBrushParams;
  FFigures.AddFigure(FFigure);
end;

procedure TShapeTool.MouseMove(APoint: TDoublePoint; AShift: TShiftState);
begin
  FFigure.Points[1]:= APoint;
end;

procedure TRegularPolygonTool.ParamAngleCountChange(Sender: TObject);
begin
  FParamAngleCount:= TSpinEdit(Sender).Value;
end;

constructor TRegularPolygonTool.Create;
begin
  inherited Create;
  FMetadata.Name:= 'RegularPolygon';
  FMetadata.Bitmap.LoadFromFile('src/rect_tool.bmp');
  FParamAngleCount:= 3;
end;

procedure TRegularPolygonTool.SetParamsPanel(APanel: TPanel);
var
  Param: TWinControl;
  TotalLeft, i: Integer;
const
  Left: Integer = 5;
  Top: Integer = 8;
begin
  for i:= (APanel.ControlCount-1) downto 0 do
    APanel.Controls[i].Free;

  Param:= CreateParamSpinEdit(APanel, 3, 1000000, FParamAngleCount, @ParamAngleCountChange);
  Param.Parent:= APanel;
  Param.Left:= Left;
  Param.Top:= Top;
  TotalLeft:= Param.Left+Param.Width+Left;

  Param:= CreateParamSpinEdit(APanel, 1, 100, FPenParams.Width, @ParamPenWidthChange);
  Param.Parent:= APanel;
  Param.Left:= TotalLeft;
  Param.Top:= Top;
  TotalLeft:= Param.Left+Param.Width+Left;

  Param:= CreateParamComboBox(APanel, 4, Integer(FPenParams.Style), @ParamPenStyleComboBoxDrawItem, @ParamPenStyleChange);
  Param.Parent:= APanel;
  Param.Left:= TotalLeft;
  Param.Top:= Top;
  TotalLeft:= Param.Left+Param.Width+Left;

  Param:= CreateParamComboBox(APanel, 8, Integer(FBrushParams.Style), @ParamBrushStyleComboBoxDrawItem, @ParamBrushStyleChange);
  Param.Parent:= APanel;
  Param.Left:= TotalLeft;
  Param.Top:= Top;
  TotalLeft:= Param.Left+Param.Width+Left;
end;

procedure TRegularPolygonTool.MouseDown(APoint: TDoublePoint; AShift: TShiftState);
begin
  FFigure:= TPolygonFigure.Create;
  FFigure.SetPointsLength(FParamAngleCount);
  FFigure.Points[0]:= APoint;
  FFigure.Points[1]:= APoint;
  FFirstPoint:= APoint;
  TShapeFigure(FFigure).PenParams:= FPenParams;
  TShapeFigure(FFigure).BrushParams:= FBrushParams;
  FFigures.AddFigure(FFigure);
end;

procedure TRegularPolygonTool.MouseMove(APoint: TDoublePoint; AShift: TShiftState);
var
  i: Integer;
  vec: TDoublePoint;
begin
  vec:= APoint - FFirstPoint;
  for i:= 0 to FParamAngleCount-1 do begin
    FFigure.Points[i]:= vec+FFirstPoint;
    vec.Rotate(2*pi/FParamAngleCount);
  end;
end;

procedure TRoundedRectTool.ParamRoundingChange(Sender: TObject);
begin
  FParamRounding:= TSpinEdit(Sender).Value;
end;

constructor TRoundedRectTool.Create;
begin
  inherited Create;
  FMetadata.Name:= 'RoundedRect';
  FMetadata.Bitmap.LoadFromFile('src/rect_tool.bmp');
end;

procedure TRoundedRectTool.SetParamsPanel(APanel: TPanel);
var
  Param: TWinControl;
  TotalLeft, i: Integer;
const
  Left: Integer = 5;
  Top: Integer = 8;
begin
  for i:= (APanel.ControlCount-1) downto 0 do
    APanel.Controls[i].Free;

  Param:= CreateParamSpinEdit(APanel, 0, 1000, FParamRounding, @ParamRoundingChange);
  Param.Parent:= APanel;
  Param.Left:= Left;
  Param.Top:= Top;
  TotalLeft:= Param.Left+Param.Width+Left;

  Param:= CreateParamSpinEdit(APanel, 1, 100, FPenParams.Width, @ParamPenWidthChange);
  Param.Parent:= APanel;
  Param.Left:= TotalLeft;
  Param.Top:= Top;
  TotalLeft:= Param.Left+Param.Width+Left;

  Param:= CreateParamComboBox(APanel, 4, Integer(FPenParams.Style), @ParamPenStyleComboBoxDrawItem, @ParamPenStyleChange);
  Param.Parent:= APanel;
  Param.Left:= TotalLeft;
  Param.Top:= Top;
  TotalLeft:= Param.Left+Param.Width+Left;

  Param:= CreateParamComboBox(APanel, 8, Integer(FBrushParams.Style), @ParamBrushStyleComboBoxDrawItem, @ParamBrushStyleChange);
  Param.Parent:= APanel;
  Param.Left:= TotalLeft;
  Param.Top:= Top;
  TotalLeft:= Param.Left+Param.Width+Left;
end;

procedure TRoundedRectTool.MouseDown(APoint: TDoublePoint; AShift: TShiftState);
begin
  FFigure:= TRoundedRectFigure.Create;
  FFigure.SetPointsLength(2);
  FFigure.Points[0]:= APoint;
  FFigure.Points[1]:= APoint;
  FFirstPoint:= APoint;
  TShapeFigure(FFigure).PenParams:= FPenParams;
  TShapeFigure(FFigure).BrushParams:= FBrushParams;
  TRoundedRectFigure(FFigure).Rounding:= FParamRounding;
  FFigures.AddFigure(FFigure);
end;

procedure TRoundedRectTool.MouseMove(APoint: TDoublePoint; AShift: TShiftState);
var
  i: Integer;
  vec: TDoublePoint;
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

