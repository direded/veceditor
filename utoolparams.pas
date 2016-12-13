unit UToolParams;

{$mode objfpc}{$H+}

interface

uses
  Classes, FPCanvas, Graphics, Controls, SysUtils, StdCtrls, Spin, UFigureParams, Dialogs, UParamEditors;

type

  TToolParam = class
  strict protected
    UISettings: record
      Left: Integer;
      Top: Integer;
    end;
    public
    constructor Create;
    procedure SetUISettings(ALeft, ATop: Integer);
    procedure FillUserInterface(AControl: TWinControl);
    procedure FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer); virtual; abstract;
  end;

  TToolParamArray = array of TToolParam;

  TSelectToolParam = class(TToolParam)
  strict protected
    FSelectAllBtnClick: TParamChangingEvent;
  public
    constructor Create;
    property SelectAllBtnClick: TParamChangingEvent read FSelectAllBtnClick write FSelectAllBtnClick;
    procedure FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer); override;
  end;

  TZoomToolParam = class(TToolParam)
  strict protected
  type
    TZoomModes = (zmtlZoomIn, zmtlZoomOut, zmtlZoomSpace);
  var
    FMode: TZoomModes;
    FZoomPerClick: Double;
    procedure FModeChange(Sender: TObject);
    procedure FZoomPerClickChange(Sender: TObject);
  public
    constructor Create;
    property Mode: TZoomModes read FMode write FMode;
    property ZoomPerClick: Double read FZoomPerClick write FZoomPerClick;
    procedure FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer); override;
  end;

  TLineToolParam = class(TToolParam)
  strict protected
    FPen: TPenParams;
    procedure FPenStyleChange(Sender: TObject);
    procedure FPenWidthChange(Sender: TObject);
    procedure FPenStyleComboBoxDrawItem(Control: TWinControl; AIndex: Integer; ARect: TRect;
                                        AState: TOwnerDrawState);
  public
    constructor Create;
    property Pen: TPenParams read FPen write FPen;
    procedure FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer); override;
  end;

  TShapeToolParam = class(TToolParam)
  strict protected
    FBrush: TBrushParams;
    procedure FBrushStyleChange(Sender: TObject);
    procedure FBrushStyleComboBoxDrawItem(Control: TWinControl; AIndex: Integer; ARect: TRect;
                                          AState: TOwnerDrawState);
  public
    constructor Create;
    property Brush: TBrushParams read FBrush write FBrush;
    procedure FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer); override;
  end;

  TRegularPolygonToolParam = class(TToolParam)
  strict protected
    FAngleCount: Integer;
    procedure FAngleCountChange(Sender: TObject);
  public
    constructor Create;
    property AngleCount: Integer read FAngleCount write FAngleCount;
    procedure FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer); override;
  end;

  TRoundedRectToolParam = class(TToolParam)
  strict protected
    FRounding: Integer;
    procedure FRoundingChange(Sender: TObject);
  public
    constructor Create;
    property Rounding: Integer read FRounding write FRounding;
    procedure FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer); override;
  end;

  procedure RegisterToolParam(AParam: TToolParam);

var
  ToolParams: TToolParamArray;

implementation

constructor TToolParam.Create;
begin
  UISettings.Left:= 5;
  UISettings.Top:= 8;
end;

procedure TToolParam.SetUISettings(ALeft, ATop: Integer);
begin
  UISettings.Left:= ALeft;
  UISettings.Top:= ATop;
end;

procedure TToolParam.FillUserInterface(AControl: TWinControl);
var
  CurrentLeft: Integer;
begin
  if AControl.ControlCount>0 then
    with AControl.Controls[AControl.ControlCount-1] do
      CurrentLeft:= Left+Width+UISettings.Left
  else
    CurrentLeft:= UISettings.Left;
  FillUserInterfaceRaw(AControl, CurrentLeft);
end;

constructor TSelectToolParam.Create;
begin
  inherited Create;
end;

procedure TSelectToolParam.FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer);
var
  Btn: TButton;
begin
  Btn:= TButton.Create(AControl);
  Btn.Parent:= AControl;
  Btn.Left:= ALeft;
  Btn.Top:= UISettings.Top;
  Btn.Caption:= 'Select all';
  Btn.OnClick:= FSelectAllBtnClick;
end;

procedure TZoomToolParam.FModeChange(Sender: TObject);
begin
  FMode:= TZoomModes(TComboBox(Sender).ItemIndex);
end;

procedure TZoomToolParam.FZoomPerClickChange(Sender: TObject);
begin
  FZoomPerClick:= TFloatSpinEdit(Sender).Value;
end;

constructor TZoomToolParam.Create;
begin
  inherited Create;
  FZoomPerClick:= 0.25;
end;

procedure TZoomToolParam.FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer);
const
  ZoomModes: array[0..2] of String = ('Zoom in', 'Zoom out', 'Zoom space');
var
  TotalLeft: Integer;
  Param: TWinControl;
begin
  TotalLeft:= ALeft;
  Param:= CreateComboBox(AControl, Point(TotalLeft, UISettings.Top), ZoomModes, Integer(FMode), @FModeChange);
  TotalLeft:= TotalLeft + Param.Width + UISettings.Left;
  CreateFloatSpinEdit(AControl, Point(TotalLeft, UISettings.Top), 0.1, 2, FZoomPerClick, @FZoomPerClickChange);
end;

procedure TLineToolParam.FPenStyleChange(Sender: TObject);
begin
  FPen.Style:= TFPPenStyle(TComboBox(Sender).ItemIndex);
end;

procedure TLineToolParam.FPenWidthChange(Sender: TObject);
begin
  FPen.Width:= TSpinEdit(Sender).Value;
end;

procedure TLineToolParam.FPenStyleComboBoxDrawItem(Control: TWinControl; AIndex: Integer; ARect: TRect;
                                                             AState: TOwnerDrawState);
const
  PenStyles: array[0..4] of TFPPenStyle = (psSolid, psDash, psDot, psDashDot, psDashDotDot);
begin
  if not AIndex in [Low(PenStyles)..High(PenStyles)] then Exit;
  TComboBox(Control).Canvas.Pen.Style:= PenStyles[AIndex];
  TComboBox(Control).Canvas.Line(ARect.Left+7, ARect.Bottom div 2+1, ARect.Right-7, ARect.Bottom div 2+1);
end;

constructor TLineToolParam.Create;
begin
  inherited Create;
  FPen.Width:= 1;
end;

procedure TLineToolParam.FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer);
var
  TotalLeft: Integer;
  Param: TWinControl;
begin
  TotalLeft:= ALeft;
  Param:= CreateSpinEdit(AControl, Point(TotalLeft, UISettings.Top), 1, 100, FPen.Width, @FPenWidthChange);
  TotalLeft:= TotalLeft + Param.Width + UISettings.Left;
  CreateComboBox(AControl, Point(TotalLeft, UISettings.Top), 5, Integer(FPen.Style), @FPenStyleComboBoxDrawItem,
                 @FPenStyleChange);
end;

procedure TShapeToolParam.FBrushStyleChange(Sender: TObject);
begin
  FBrush.Style:= TFPBrushStyle(TComboBox(Sender).ItemIndex);
end;

procedure TShapeToolParam.FBrushStyleComboBoxDrawItem(Control: TWinControl; AIndex: Integer; ARect: TRect;
                                                                 AState: TOwnerDrawState);
const
  BrushStyles: array[0..7] of TFPBrushStyle = (bsSolid, bsClear, bsHorizontal, bsVertical, bsFDiagonal,
    bsBDiagonal, bsCross, bsDiagCross);
begin
  ARect.Left+= 8;
  ARect.Right-= 8;
  ARect.Top+= 4;
  ARect.Bottom-= 3;
  if AIndex in [Low(BrushStyles)..High(BrushStyles)] then
    TComboBox(Control).Canvas.Brush.Style:= BrushStyles[AIndex];
  TComboBox(Control).Canvas.Pen.Color:= clGray;
  TComboBox(Control).Canvas.Brush.Color:= clGray;
  TComboBox(Control).Canvas.Rectangle(ARect);
end;

constructor TShapeToolParam.Create;
begin
  inherited Create;
end;

procedure TShapeToolParam.FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer);
begin
  CreateComboBox(AControl, Point(ALeft, UISettings.Top), 8, Integer(FBrush.Style),
                 @FBrushStyleComboBoxDrawItem, @FBrushStyleChange);
end;

procedure TRegularPolygonToolParam.FAngleCountChange(Sender: TObject);
begin
  FAngleCount:= TSpinEdit(Sender).Value;
end;

constructor TRegularPolygonToolParam.Create;
begin
  inherited Create;
end;

procedure TRegularPolygonToolParam.FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer);
begin
  CreateSpinEdit(AControl, Point(ALeft, UISettings.Top), 3, 1000, FAngleCount, @FAngleCountChange);
end;

procedure TRoundedRectToolParam.FRoundingChange(Sender: TObject);
begin
  FRounding:= TSpinEdit(Sender).Value;
end;

constructor TRoundedRectToolParam.Create;
begin
  inherited Create;
end;

procedure TRoundedRectToolParam.FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer);
begin
  CreateSpinEdit(AControl, Point(ALeft, UISettings.Top), 0, 1000, FRounding, @FRoundingChange);
end;

procedure RegisterToolParam(AParam: TToolParam);
begin
  SetLength(ToolParams, Length(ToolParams)+1);
  ToolParams[High(ToolParams)]:= AParam;
end;

initialization

RegisterToolParam(TSelectToolParam.Create);
RegisterToolParam(TZoomToolParam.Create);
RegisterToolParam(TLineToolParam.Create);
RegisterToolParam(TShapeToolParam.Create);
RegisterToolParam(TRegularPolygonToolParam.Create);
RegisterToolParam(TRoundedRectToolParam.Create);

end.

