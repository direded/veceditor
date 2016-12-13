unit UTools;

{$mode objfpc}{$H+}

interface
uses
	Classes, Types, Controls, Graphics,
  ExtCtrls, StdCtrls, UFigures, UDoublePoint, UPaintSpace, Math,
  UFigureParams, UToolParams, UParamEditors, UUtils;

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
    FOnParamChange: TEventHandler;
    FOnParamsListChange: TEventHandler;
  public
    constructor Create;
    property OnParamChange: TEventHandler read FOnParamChange write FOnParamChange;
    property OnParamsListChange: TEventHandler read FOnParamsListChange write FOnParamsListChange;
    property Figures: TFigures read FFigures write FFigures;
    property Metadata: TToolMetadata read FMetadata;
    property PaintSpace: TPaintSpace write FPaintSpace;
    class procedure CleanParamsPanel(APanel: TWinControl);
    procedure SetParamColor(AFigureColors: TFigureColors); virtual;
    procedure SetParamsPanel(APanel: TPanel);
    function GetParams: TParamEditorArray; virtual;
    procedure CleanUp; virtual;
    procedure MouseDown(APoint: TDoublePoint; AShift: TShiftState); virtual; abstract;
    procedure MouseMove(APoint: TDoublePoint; AShift: TShiftState); virtual; abstract;
  	procedure MouseUp(APoint:TDoublePoint; AShift: TShiftState); virtual; abstract;
  end;

  TZoomTool = class(TTool)
  strict private
    FModeE: TZoomModePEditor;
    FPowerE: TZoomPowerPEditor;
    FFirstPoint: TDoublePoint;
    FSplitOff: TRectSplitOffFigure;
  public
    constructor Create;
    function GetParams: TParamEditorArray; override;
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
    FSelectBtnE: TSelectAllPEditor;
    FFirstPoint: TDoublePoint;
    FSplitOff: TRectSplitOffFigure;
    FIsFirstOnFigure: Boolean;
    FLastPoint: TDoublePoint;
    FCommonParams: array of TFigureParamArray;
    FFigureEditors: TFigureParamEditorArray;
    procedure ParamChange;
    procedure FRevers(AFigure: TFigure);
    procedure FSelect(AFigure: TFigure);
    procedure FSelectAllBtnClick(Sender: TObject);
    procedure InitFiguresParams;
    procedure AddFigureParamEditor(AParam: TFigureParamEditor);
    procedure CrossParams(AParams: TFigureParamArray);
    procedure ShowFiguresEditors(AControl: TWinControl);
    const
      CLICK_SIZE: Integer = 3;
  public
  	constructor Create;
    function GetParams: TParamEditorArray; override;
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
    FLineWidthE: TLineWidthPEditor;
    FLineStyleE: TLineStylePEditor;
    FLineColorE: TColorPEditor;
    procedure CreateFigure; override;
    procedure InitializeFigure(APoint: TDoublePoint); override;
    procedure SetFigureParams; override;
  public
    constructor Create;
    function GetParams: TParamEditorArray; override;
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
    FLineWidthE: TLineWidthPEditor;
    FLineStyleE: TLineStylePEditor;
    FLineColorE: TColorPEditor;
    FShapeColorE: TColorPEditor;
    FShapeStyleE: TShapeStylePEditor;
    procedure InitializeFigure(APoint: TDoublePoint); override;
    procedure SetFigureParams; override;
  public
    constructor Create;
    function GetParams: TParamEditorArray; override;
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
  strict protected
    FAngleCountE: TAngleCountPEditor;
    procedure CreateFigure; override;
  public
    constructor Create;
    function GetParams: TParamEditorArray; override;
    procedure MouseMove(APoint: TDoublePoint; AShift: TShiftState); override;
    procedure MouseDown(APoint: TDoublePoint; AShift: TShiftState); override;
  end;

  TRoundedRectTool = class(TShapeTool)
  strict protected
    FRoundingE: TRoundingPEditor;
    procedure CreateFigure; override;
    procedure SetFigureParams; override;
  public
    constructor Create;
    function GetParams: TParamEditorArray; override;
    procedure MouseMove(APoint: TDoublePoint; AShift: TShiftState); override;
    procedure MouseDown(APoint: TDoublePoint; AShift: TShiftState); override;
  end;

	procedure RegisterTool(ATool: TTool);

var
	Tools: array of TTool;

implementation

procedure Push(var Element: TFigureParam; var Arr: TFigureParamArray);
begin
  SetLength(Arr, Length(Arr)+1);
  Arr[High(Arr)]:= Element;
end;

procedure Push(Element: Integer; var Arr: TIntArray);
begin
  SetLength(Arr, Length(Arr)+1);
  Arr[High(Arr)]:= Element;
end;

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
var p: TParamEditor;
begin
  CleanParamsPanel(APanel);
  for p in GetParams do
    p.FillUserInterface(APanel);
end;

function TTool.GetParams: TParamEditorArray;
begin

end;

procedure TTool.CleanUp;
begin

end;

procedure TSelectTool.ParamChange;
begin
  FPaintSpace.PaintBox.Invalidate;
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
  FSelectBtnE:= TSelectAllPEditor.Create;
  FSelectBtnE.SelectAllBtnClick:= @FSelectAllBtnClick;
  FMetadata.Bitmap.LoadFromFile('src/select_tool.bmp');
end;

procedure TSelectTool.InitFiguresParams;
var
  i, j: Integer;
  firstFound: Boolean = false;
  fp: TFigureParamArray;
  editors: TFigureParamEditorClassArray;
  editor: TFigureParamEditor;
begin
  FFigureEditors:= nil;
  FCommonParams:= nil;
  for i:= 0 to High(FFigures.Content) do begin
    if FFigures[i].Selected then begin
      fp:= Figures[i].GetParams;
      if not firstFound then begin
        firstFound:= True;
        SetLength(FCommonParams, Length(fp));
        for j:= 0 to High(FCommonParams) do begin
          SetLength(FCommonParams[j], 1);
          FCommonParams[j, 0]:= fp[j];
        end;
      end
      else
        CrossParams(fp);
    end;
  end;
  editors:= GetFigureEditorClasses;
  for i:= Low(FCommonParams) to High(FCommonParams) do begin
    //writeln(Length(FCommonParams));
    for j:= 0 to High(editors) do begin
      editor:= editors[j].Create;
      if FCommonParams[i,0].ClassType = editor.GetParamType then begin
        editor.AttachParams(FCommonParams[i]);
        AddFigureParamEditor(editor);
        editor.OnParamChange:= @ParamChange;
        //writeln(editor.GetParamType.ClassName);
      end
      else
        editor.Free;
    end;
  end;
end;

procedure TSelectTool.AddFigureParamEditor(AParam: TFigureParamEditor);
begin
  SetLength(FFigureEditors, Length(FFigureEditors)+1);
  FFigureEditors[High(FFigureEditors)]:= AParam;
end;

procedure TSelectTool.CrossParams(AParams: TFigureParamArray);
var
  i, j: Integer;
  IsFound: Boolean;
  TempParams: array of array of TFigureParam;
  DiffIndexes: TIntArray;
begin
  for i:= Low(FCommonParams) to High(FCommonParams) do begin
    IsFound:= false;
    for j:= Low(AParams) to High(AParams) do begin
      if FCommonParams[i, 0].ClassType = AParams[j].ClassType then begin
        IsFound:= true;
        Push(AParams[j], FCommonParams[i]);
      end;
    end;
    if not IsFound then begin
      Push(i, DiffIndexes);
    end;
  end;
  SetLength(TempParams, Length(FCommonParams)-Length(DiffIndexes));
  j:= Low(TempParams);
  for i:= Low(FCommonParams) to High(FCommonParams) do begin
    if IsInArray(i, DiffIndexes) then Continue;
    TempParams[j]:= FCommonParams[i];
    j+= 1;
  end;
  FCommonParams:= TempParams;
end;

procedure TSelectTool.ShowFiguresEditors(AControl: TWinControl);
var
  e: TFigureParamEditor;
begin
  for e in FFigureEditors do
    e.FillUserInterface(AControl);
end;

function TSelectTool.GetParams: TParamEditorArray;
var
  i: Integer;
begin
  SetLength(Result, 1);
  Result[0]:= FSelectBtnE;
  //SetLength(Result, Length(Result)+Length(FFigureEditors));
  for i:= Low(FFigureEditors) to High(FFigureEditors) do begin
    SetLength(Result, Length(Result)+1);
    Result[High(Result)]:= FFigureEditors[i];
  end;
end;

procedure TSelectTool.CleanUp;
begin
  FFigures.UnSelectAllFigures;
  FPaintSpace.PaintBox.Invalidate;
  FFigureEditors:= nil;
  FCommonParams:= nil;
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
  InitFiguresParams;
  if Assigned(FOnParamsListChange) then FOnParamsListChange;
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
  FModeE:= TZoomModePEditor.Create;
  FPowerE:= TZoomPowerPEditor.Create;
  FMetadata.Name:= 'Zoom';
  FMetadata.Bitmap.LoadFromFile('src/zoom_tool.bmp');
end;

function TZoomTool.GetParams: TParamEditorArray;
begin
  SetLength(Result, 2);
  Result[0]:= FPowerE;
  Result[1]:= FModeE;
end;

procedure TZoomTool.MouseDown(APoint: TDoublePoint; AShift: TShiftState);
begin
  FFirstPoint:= APoint;
  if FModeE.Parameter.Value = TZoomModeParam.TZoomModes.zmtlZoomSpace then begin
    FSplitOff:= TRectSplitOffFigure.Create;
    FSplitOff.SetPointsLength(2);
    FSplitOff.Points[0]:= APoint;
    FFigures.AddFigure(FSplitOff);
  end;
end;

procedure TZoomTool.MouseMove(APoint: TDoublePoint; AShift: TShiftState);
begin
  if FModeE.Parameter.Value = TZoomModeParam.TZoomModes.zmtlZoomSpace then begin
    FSplitOff.Points[1]:= APoint;
  end;
end;

procedure TZoomTool.MouseUp(APoint: TDoublePoint; AShift: TShiftState);
begin
  if ssRight in AShift then exit;
  case FModeE.Parameter.Value  of
    TZoomModeParam.TZoomModes.zmtlZoomOut : FPaintSpace.SetScalePoint(FPaintSpace.Scale-FPowerE.Parameter.Value, APoint);
    TZoomModeParam.TZoomModes.zmtlZoomIn : FPaintSpace.SetScalePoint(FPaintSpace.Scale+FPowerE.Parameter.Value, APoint);
    TZoomModeParam.TZoomModes.zmtlZoomSpace : begin
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
  inherited;
  FLineColorE:= TColorPEditor.Create;
  FLineWidthE:= TLineWidthPEditor.Create;
  FLineStyleE:= TLineStylePEditor.Create;
  FMetadata.Name:= 'Line';
  FMetadata.Bitmap:= TBitmap.Create;
  FMetadata.Bitmap.LoadFromFile('src/line_tool.bmp');
end;

function TLineTool.GetParams: TParamEditorArray;
begin
  SetLength(Result, 3);
  Result[0]:= FLineWidthE;
  Result[1]:= FLineStyleE;
  Result[2]:= FLineColorE;
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
var
  f: TLineFigure;
begin
  f:= TLineFigure(FFigure);
  f.LineWidth.Value:= FLineWidthE.Parameter.Value;
  f.LineStyle.Value:= FLineStyleE.Parameter.Value;
  f.LineColor.Value:= FLineColorE.Parameter.Value;
end;

procedure TLineTool.SetParamColor(AFigureColors: TFigureColors);
begin
  FLineColorE.Parameter.Value:= AFigureColors.Pen;
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
  inherited Create;
  FMetadata.Name:= 'Pen';
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
var
  f: TShapeFigure;
begin
  f:= TShapeFigure(FFigure);
  f.LineColor.Value:= FLineColorE.Parameter.Value;
  f.LineWidth.Value:= FLineWidthE.Parameter.Value;
  f.LineStyle.Value:= FLineStyleE.Parameter.Value;
  f.ShapeColor.Value:= FShapeColorE.Parameter.Value;
  f.ShapeStyle.Value:= FShapeStyleE.Parameter.Value;
end;

constructor TShapeTool.Create;
begin
  inherited;
  FLineWidthE:= TLineWidthPEditor.Create;
  FLineStyleE:= TLineStylePEditor.Create;
  FLineColorE:= TColorPEditor.Create;
  FShapeColorE:= TColorPEditor.Create;
  FShapeStyleE:= TShapeStylePEditor.Create;
end;

function TShapeTool.GetParams: TParamEditorArray;
begin
  SetLength(Result, 5);
  Result[0]:= FLineWidthE;
  Result[1]:= FLineStyleE;
  Result[2]:= FLineColorE;
  Result[3]:= FShapeStyleE;
  Result[4]:= FShapeColorE;
end;

procedure TShapeTool.SetParamColor(AFigureColors: TFigureColors);
begin
  FLineColorE.Parameter.Value:= AFigureColors.Pen;
  FShapeColorE.Parameter.Value:= AFigureColors.Brush;
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
  inherited;
  FMetadata.Name:= 'Ellipse';
  FMetadata.Bitmap.LoadFromFile('src/ellipse_tool.bmp');
end;

procedure TRectTool.CreateFigure;
begin
  FFigure:= TRectFigure.Create;
  FFigures.AddFigure(FFigure);
end;

constructor TRectTool.Create;
begin
  inherited;
  FMetadata.Name:= 'Rect';
  FMetadata.Bitmap.LoadFromFile('src/rect_tool.bmp');
end;

procedure TRegularPolygonTool.CreateFigure;
begin
  FFigure:= TRegularPolygonFigure.Create;
  FFigure.SetPointsLength(FAngleCountE.Parameter.Value);
  FFigures.AddFigure(FFigure);
end;

constructor TRegularPolygonTool.Create;
begin
  inherited;
  FMetadata.Name:= 'RegularPolygon';
  FAngleCountE:= TAngleCountPEditor.Create;
  FMetadata.Bitmap.LoadFromFile('src/regular_tool.bmp');
  FAngleCountE.Parameter.Value:= 3;
end;

function TRegularPolygonTool.GetParams: TParamEditorArray;
begin
  Result:= inherited GetParams;
  SetLength(Result, Length(Result)+1);
  Result[High(Result)]:= FAngleCountE;
end;

procedure TRegularPolygonTool.MouseDown(APoint: TDoublePoint; AShift: TShiftState);
begin
  CreateFigure;
  InitializeFigure(APoint);
  SetFigureParams;
  FFigure.Points[1]:= APoint;
  TRegularPolygonFigure(FFigure).AngleCountParam.Value:= FAngleCountE.Parameter.Value;
end;

procedure TRegularPolygonTool.MouseMove(APoint: TDoublePoint; AShift: TShiftState);
begin
  FFigure.Points[1]:= APoint;
end;

procedure TRoundedRectTool.CreateFigure;
begin
  FFigure:= TRoundedRectFigure.Create;
  FFigures.AddFigure(FFigure);
end;

procedure TRoundedRectTool.SetFigureParams;
begin
  inherited SetFigureParams;
  TRoundedRectFigure(FFigure).Rounding:= FRoundingE.Parameter.Value;
end;

constructor TRoundedRectTool.Create;
begin
  inherited;
  FRoundingE:= TRoundingPEditor.Create;
  FMetadata.Name:= 'RoundedRect';
  FMetadata.Bitmap.LoadFromFile('src/rect_tool.bmp');
end;

function TRoundedRectTool.GetParams: TParamEditorArray;
begin
  Result:= inherited GetParams;
  SetLength(Result, Length(Result)+1);
  Result[High(Result)]:= FRoundingE;
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

