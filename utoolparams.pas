unit UToolParams;

{$mode objfpc}{$H+}

interface

uses
  Classes, FPCanvas, Graphics, Controls, SysUtils, StdCtrls, Spin, UFigureParams, Dialogs;

type

  TParamChangingEvent = procedure(Sender: TObject) of object;
  TDrawItemEvent = procedure(Control: TWinControl; AIndex: Integer; ARect: TRect;
                             AState: TOwnerDrawState) of object;

  TToolParameters = class
  strict protected
    UISettings: record
      Left: Integer;
      Top: Integer;
    end;
    class function CreateComboBox(AOwner: TWinControl; APoint: TPoint; AItems: array of String;
                                  AStartValue: Integer; AEvent: TParamChangingEvent): TComboBox;
    class function CreateComboBox(AOwner: TWinControl; APoint: TPoint; AItemCount: Integer; AStartValue: Integer;
                                  ADrawItemEvent: TDrawItemEvent; AChangingEvent: TParamChangingEvent): TComboBox;
    class function CreateSpinEdit(AOwner: TWinControl; APoint: TPoint; AMin, AMax: Integer;
                                  AStartValue: Integer; AEvent: TParamChangingEvent): TSpinEdit;
    class function CreateFloatSpinEdit(AOwner: TWinControl; APoint: TPoint; AMin, AMax: Double;
                                       AStartValue: Double; AEvent: TParamChangingEvent): TFloatSpinEdit;
  public
    constructor Create;
    procedure SetUISettings(ALeft, ATop: Integer);
    procedure FillUserInterface(AControl: TWinControl); virtual; abstract;
  end;

  TZoomToolParameters = class(TToolParameters)
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
    procedure FillUserInterface(AControl: TWinControl); override;
  end;

  TLineToolParameters = class(TToolParameters)
  strict protected
    FPen: TPenParams;
    procedure FPenStyleChange(Sender: TObject);
    procedure FPenWidthChange(Sender: TObject);
    procedure FPenStyleComboBoxDrawItem(Control: TWinControl; AIndex: Integer; ARect: TRect;
                                        AState: TOwnerDrawState);
  public
    constructor Create;
    property Pen: TPenParams read FPen write FPen;
    procedure FillUserInterface(AControl: TWinControl); override;
  end;

  TShapeToolParameters = class(TLineToolParameters)
  strict protected
    FBrush: TBrushParams;
    procedure FBrushStyleChange(Sender: TObject);
    procedure FBrushStyleComboBoxDrawItem(Control: TWinControl; AIndex: Integer; ARect: TRect;
                                          AState: TOwnerDrawState);
  public
    constructor Create;
    property Brush: TBrushParams read FBrush write FBrush;
    procedure FillUserInterface(AControl: TWinControl); override;
  end;

  TRegularPolygonToolParameters = class(TShapeToolParameters)
  strict protected
    FAngleCount: Integer;
    procedure FAngleCountChange(Sender: TObject);
  public
    constructor Create;
    property AngleCount: Integer read FAngleCount write FAngleCount;
    procedure FillUserInterface(AControl: TWinControl); override;
  end;

  TRoundedRectToolParameters = class(TShapeToolParameters)
  strict protected
    FRounding: Integer;
    procedure FRoundingChange(Sender: TObject);
  public
    constructor Create;
    property Rounding: Integer read FRounding write FRounding;
    procedure FillUserInterface(AControl: TWinControl); override;
  end;


implementation

class function TToolParameters.CreateComboBox(AOwner: TWinControl; APoint: TPoint; AItems: array of String;
                                              AStartValue: Integer; AEvent: TParamChangingEvent): TComboBox;
var
  str: String;
begin
  if Length(AItems) = 0 then Exit;
  Result:= TComboBox.Create(AOwner);
  with Result do begin
    Left:= APoint.X;
    Top:= APoint.Y;
    Parent:= AOwner;
    Width:= 80;
    ReadOnly:= true;
    for str in AItems do
      Items.Add(str);
    ItemIndex:= AStartValue;
    OnChange:= AEvent;
  end;
end;

class function TToolParameters.CreateComboBox(AOwner: TWinControl; APoint: TPoint; AItemCount: Integer; AStartValue: Integer;
                                              ADrawItemEvent: TDrawItemEvent; AChangingEvent: TParamChangingEvent): TComboBox;
var
  i: Integer;
begin
  if AItemCount <= 0 then Exit;
  Result:= TComboBox.Create(AOwner);
  with Result do begin
    Left:= APoint.X;
    Top:= APoint.Y;
    Parent:= AOwner;
    Width:= 80;
    ReadOnly:= true;
    for i:= 0 to AItemCount-1 do
      Items.Add('');
    ItemIndex:= AStartValue;
    Style:= csOwnerDrawFixed;
    OnChange:= AChangingEvent;
    OnDrawItem:= ADrawItemEvent;
  end;
end;

class function TToolParameters.CreateSpinEdit(AOwner: TWinControl; APoint: TPoint; AMin, AMax: Integer;
                                              AStartValue: Integer; AEvent: TParamChangingEvent): TSpinEdit;
begin
  Result:= TSpinEdit.Create(AOwner);
  with Result do begin
    Left:= APoint.X;
    Top:= APoint.Y;
    Parent:= AOwner;
    Width:= 50;
    OnChange:= AEvent;
    Value:= AStartValue;
    MaxValue:= AMax;
    MinValue:= AMin;
  end;
end;

class function TToolParameters.CreateFloatSpinEdit(AOwner: TWinControl; APoint: TPoint; AMin, AMax: Double;
                                              AStartValue: Double; AEvent: TParamChangingEvent): TFloatSpinEdit;
begin
  Result:= TFloatSpinEdit.Create(AOwner);
  with Result do begin
    Left:= APoint.X;
    Top:= APoint.Y;
    Parent:= AOwner;
    Width:= 85;
    OnChange:= AEvent;
    Value:= AStartValue;
    MaxValue:= AMax;
    MinValue:= AMin;
  end;
end;

constructor TToolParameters.Create;
begin
  UISettings.Left:= 5;
  UISettings.Top:= 8;
end;

procedure TToolParameters.SetUISettings(ALeft, ATop: Integer);
begin
  UISettings.Left:= ALeft;
  UISettings.Top:= ATop;
end;

procedure TZoomToolParameters.FModeChange(Sender: TObject);
begin
  FMode:= TZoomModes(TComboBox(Sender).ItemIndex);
end;

procedure TZoomToolParameters.FZoomPerClickChange(Sender: TObject);
begin
  FZoomPerClick:= TFloatSpinEdit(Sender).Value;
end;

constructor TZoomToolParameters.Create;
begin
  inherited Create;
  FZoomPerClick:= 0.25;
end;

procedure TZoomToolParameters.FillUserInterface(AControl: TWinControl);
const
  ZoomModes: array[0..2] of String = ('Zoom in', 'Zoom out', 'Zoom space');
var
  TotalLeft: Integer;
  Param: TWinControl;
begin
  TotalLeft:= UISettings.Left;
  Param:= CreateComboBox(AControl, Point(TotalLeft, UISettings.Top), ZoomModes, Integer(FMode), @FModeChange);
  TotalLeft:= TotalLeft + Param.Width + UISettings.Left;
  Param:= CreateFloatSpinEdit(AControl, Point(TotalLeft, UISettings.Top), 0.1, 2, FZoomPerClick, @FZoomPerClickChange);
end;

procedure TLineToolParameters.FPenStyleChange(Sender: TObject);
begin
  FPen.Style:= TFPPenStyle(TComboBox(Sender).ItemIndex);
end;

procedure TLineToolParameters.FPenWidthChange(Sender: TObject);
begin
  FPen.Width:= TSpinEdit(Sender).Value;
end;

procedure TLineToolParameters.FPenStyleComboBoxDrawItem(Control: TWinControl; AIndex: Integer; ARect: TRect;
                                                             AState: TOwnerDrawState);
const
  PenStyles: array[0..4] of TFPPenStyle = (psSolid, psDash, psDot, psDashDot, psDashDotDot);
begin
  if not AIndex in [Low(PenStyles)..High(PenStyles)] then Exit;
  TComboBox(Control).Canvas.Pen.Style:= PenStyles[AIndex];
  TComboBox(Control).Canvas.Line(ARect.Left+7, ARect.Bottom div 2+1, ARect.Right-7, ARect.Bottom div 2+1);
end;

constructor TLineToolParameters.Create;
begin
  inherited Create;
  FPen.Width:= 1;
end;

procedure TLineToolParameters.FillUserInterface(AControl: TWinControl);
var
  TotalLeft: Integer;
  Param: TWinControl;
begin
  TotalLeft:= UISettings.Left;
  Param:= CreateSpinEdit(AControl, Point(TotalLeft, UISettings.Top), 1, 100, FPen.Width, @FPenWidthChange);
  TotalLeft:= TotalLeft + Param.Width + UISettings.Left;
  Param:= CreateComboBox(AControl, Point(TotalLeft, UISettings.Top), 5, Integer(FPen.Style), @FPenStyleComboBoxDrawItem,
    @FPenStyleChange);
end;

procedure TShapeToolParameters.FBrushStyleChange(Sender: TObject);
begin
  FBrush.Style:= TFPBrushStyle(TComboBox(Sender).ItemIndex);
end;

procedure TShapeToolParameters.FBrushStyleComboBoxDrawItem(Control: TWinControl; AIndex: Integer; ARect: TRect;
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

constructor TShapeToolParameters.Create;
begin
  inherited Create;
end;

procedure TShapeToolParameters.FillUserInterface(AControl: TWinControl);
var
 TotalLeft: Integer;
 Param: TWinControl;
begin
  inherited FillUserInterface(AControl);
  with AControl.Controls[AControl.ControlCount-1] do
    TotalLeft:= Left+Width+UISettings.Left;
  Param:= CreateComboBox(AControl, Point(TotalLeft, UISettings.Top), 8, Integer(FBrush.Style),
    @FBrushStyleComboBoxDrawItem, @FBrushStyleChange);
end;

procedure TRegularPolygonToolParameters.FAngleCountChange(Sender: TObject);
begin
  FAngleCount:= TSpinEdit(Sender).Value;
end;

constructor TRegularPolygonToolParameters.Create;
begin
  inherited Create;
end;

procedure TRegularPolygonToolParameters.FillUserInterface(AControl: TWinControl);
var
  Param: TWinControl;
begin
  inherited FillUserInterface(AControl);
  Param:= CreateSpinEdit(AControl, Point(UISettings.Left, UISettings.Top), 3, 1000, FAngleCount, @FAngleCountChange);
end;

procedure TRoundedRectToolParameters.FRoundingChange(Sender: TObject);
begin
  FRounding:= TSpinEdit(Sender).Value;
end;

constructor TRoundedRectToolParameters.Create;
begin
  inherited Create;
end;

procedure TRoundedRectToolParameters.FillUserInterface(AControl: TWinControl);
var
  Param: TWinControl;
begin
  inherited FillUserInterface(AControl);
  Param:= CreateSpinEdit(AControl, Point(UISettings.Left, UISettings.Top), 0, 1000, FRounding, @FRoundingChange);
end;

end.

