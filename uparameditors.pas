unit UParamEditors;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, SysUtils, StdCtrls, FPCanvas, Spin, UFigureParams, Graphics, UUtils;

type

  TParamClass = class of TFigureParam;
  TParamClassArray = array of TParamClass;

  TParamChangingEvent = procedure(Sender: TObject) of object;
  TDrawItemEvent = procedure(Control: TWinControl; AIndex: Integer; ARect: TRect;
                             AState: TOwnerDrawState) of object;

  TParamEditor = class
  strict protected
    FOnHistoryChange: TEventHandler;
    FParam: TFigureParam;
  const
    UILeft: Integer = 5;
    UITop: Integer = 8;
  public
    property OnHistoryChange: TEventHandler read FOnHistoryChange write FOnHistoryChange;
    procedure FillUserInterface(AControl: TWinControl);
    procedure FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer); virtual; abstract;
  end;

  TParamEditorArray = array of TParamEditor;
  TParamEditorClass = class of TParamEditor;
  TParamEditorClassArray = array of TParamEditorClass;

  TZoomModePEditor = class(TParamEditor)
  strict private
    procedure ParamChange(Sender: TObject);
    function GetParam: TZoomModeParam;
  public
    constructor Create;
    property Parameter: TZoomModeParam read GetParam;
    procedure FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer); override;
  end;

  TZoomPowerPEditor = class(TParamEditor)
  strict private
    procedure ParamChange(Sender: TObject);
    function GetParam: TZoomPowerParam;
  public
    constructor Create;
    property Parameter: TZoomPowerParam read GetParam;
    procedure FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer); override;
  end;

  TSelectAllPEditor = class(TParamEditor)
  strict private
    FSelectAllBtnClick: TParamChangingEvent;
  public
    property SelectAllBtnClick: TParamChangingEvent read FSelectAllBtnClick write FSelectAllBtnClick;
    procedure FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer); override;
  end;

  TFigureParamEditor = class (TParamEditor)
  strict protected
    IsFirstChange: Boolean;
    FParams: TFigureParamArray;
    FOnParamChange: TEventHandler;
    IsParamRealChanges: Boolean;
  public
    property OnParamChange: TEventHandler read FOnParamChange write FOnParamChange;
    procedure AttachParams(AParams: TFigureParamArray);
    function GetParamType: TFigureParamClass; virtual; abstract;
  end;

  TFigureParamEditorArray = array of TFigureParamEditor;
  TFigureParamEditorClass = class of TFigureParamEditor;
  TFigureParamEditorClassArray = array of TFigureParamEditorClass;

  TColorPEditor = class(TFigureParamEditor)
  strict private
    function GetParam: TColorParam;
  public
    constructor Create;
    property Parameter: TColorParam read GetParam;
    function GetParamType: TFigureParamClass; override;
    procedure FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer); override;
  end;

  TLineWidthPEditor = class(TFigureParamEditor)
  strict private
    procedure ParamChange(Sender: TObject);
    function GetParam: TLineWidthParam;
  public
    constructor Create;
    property Parameter: TLineWidthParam read GetParam;
    function GetParamType: TFigureParamClass; override;
    procedure FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer); override;
  end;

  TLineStylePEditor = class(TFigureParamEditor)
  strict private
    function GetParam: TLineStyleParam;
    procedure ParamComboBoxDrawItem(Control: TWinControl; AIndex: Integer; ARect: TRect;
                                    AState: TOwnerDrawState);
    procedure ParamChange(Sender: TObject);
  public
    constructor Create;
    property Parameter: TLineStyleParam read GetParam;
    function GetParamType: TFigureParamClass; override;
    procedure FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer); override;
  end;

  TShapeStylePEditor = class(TFigureParamEditor)
  strict private
    function GetParam: TShapeStyleParam;
    procedure ParamChange(Sender: TObject);
    procedure ParamComboBoxDrawItem(Control: TWinControl; AIndex: Integer; ARect: TRect;
                                    AState: TOwnerDrawState);
  public
    constructor Create;
    property Parameter: TShapeStyleParam read GetParam;
    function GetParamType: TFigureParamClass; override;
    procedure FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer); override;

  end;

  TAngleCountPEditor = class(TFigureParamEditor)
  strict private
    function GetParam: TAngleCountParam;
    procedure ParamChange(Sender: TObject);
  public
    constructor Create;
    property Parameter: TAngleCountParam read GetParam;
    function GetParamType: TFigureParamClass; override;
    procedure FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer); override;
  end;

  TRoundingPEditor = class(TFigureParamEditor)
  strict private
    function GetParam: TRoundingParam;
    procedure ParamChange(Sender: TObject);
  public
    constructor Create;
    property Parameter: TRoundingParam read GetParam;
    function GetParamType: TFigureParamClass; override;
    procedure FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer); override;
  end;

function CreateComboBox(AOwner: TWinControl; APoint: TPoint; AItems: array of String;
                        AStartValue: Integer; AEvent: TParamChangingEvent): TComboBox;
function CreateComboBox(AOwner: TWinControl; APoint: TPoint; AItemCount: Integer; AStartValue: Integer;
                              ADrawItemEvent: TDrawItemEvent; AChangingEvent: TParamChangingEvent): TComboBox;
function CreateSpinEdit(AOwner: TWinControl; APoint: TPoint; AMin, AMax: Integer;
                              AStartValue: Integer; AEvent: TParamChangingEvent): TSpinEdit;
function CreateFloatSpinEdit(AOwner: TWinControl; APoint: TPoint; AMin, AMax: Double;
                                   AStartValue: Double; AEvent: TParamChangingEvent): TFloatSpinEdit;
function GetFigureEditorClasses: TFigureParamEditorClassArray;

implementation

procedure TParamEditor.FillUserInterface(AControl: TWinControl);
var
  CurrentLeft: Integer;
begin
  if AControl.ControlCount>0 then
    with AControl.Controls[AControl.ControlCount-1] do
      CurrentLeft:= Left+Width+UILeft
  else
    CurrentLeft:= UILeft;
  FillUserInterfaceRaw(AControl, CurrentLeft);
end;

constructor TZoomPowerPEditor.Create;
begin
  FParam:= TZoomPowerParam.Create;
end;

procedure TZoomPowerPEditor.FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer);
begin
  CreateFloatSpinEdit(AControl, Point(ALeft, UITop), 0.1, 2, GetParam.Value, @ParamChange);
end;

procedure TZoomModePEditor.ParamChange(Sender: TObject);
begin
  GetParam.Value:= TZoomModeParam.TZoomModes(TComboBox(Sender).ItemIndex);
end;

function TZoomModePEditor.GetParam: TZoomModeParam;
begin
  Result:= TZoomModeParam(FParam);
end;

constructor TZoomModePEditor.Create;
begin
  FParam:= TZoomModeParam.Create;
end;

procedure TZoomModePEditor.FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer);
const ZoomModes: array [0..2] of String = ('Zoom in', 'Zoom out', 'Zoom space');
begin
  CreateComboBox(AControl, Point(ALeft, UITop), ZoomModes, Integer(GetParam.Value), @ParamChange);
end;

procedure TZoomPowerPEditor.ParamChange(Sender: TObject);
begin
  GetParam.Value:= TFloatSpinEdit(Sender).Value;
end;

function TZoomPowerPEditor.GetParam: TZoomPowerParam;
begin
  Result:= TZoomPowerParam(FParam);
end;

procedure TSelectAllPEditor.FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer);
var
  Btn: TButton;
begin
  Btn:= TButton.Create(AControl);
  Btn.Parent:= AControl;
  Btn.Left:= ALeft;
  Btn.Top:= UITop;
  Btn.Caption:= 'Select all';
  Btn.OnClick:= FSelectAllBtnClick;
end;

procedure TFigureParamEditor.AttachParams(AParams: TFigureParamArray);
var
  paramClass: TFigureParamClass;
begin
  if Length(FParams) = 0 then IsFirstChange:= true;
  FParams:= AParams;
  paramClass:= GetParamType;
  FParam:= AParams[0] as paramClass;
end;

function TColorPEditor.GetParam: TColorParam;
begin
  Result:= TColorParam(FParam);
end;

constructor TColorPEditor.Create;
begin
  IsFirstChange:= true;
  FParam:= TColorParam.Create;
end;

function TColorPEditor.GetParamType: TFigureParamClass;
begin
  Result:= TColorParam;
end;

procedure TColorPEditor.FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer);
begin

end;

procedure TLineWidthPEditor.ParamChange(Sender: TObject);
var
  i: Integer;
begin
  if GetParam.Value = TSpinEdit(Sender).Value then Exit;
  GetParam.Value:= TSpinEdit(Sender).Value;
  for i:= 0 to High(FParams) do
    TLineWidthParam(FParams[i]).Value:= TSpinEdit(Sender).Value;
  if Assigned(FOnParamChange) then FOnParamChange;
end;

function TLineWidthPEditor.GetParam: TLineWidthParam;
begin
  Result:= TLineWidthParam(FParam);
end;

constructor TLineWidthPEditor.Create;
begin
  IsFirstChange:= true;
  FParam:= TLineWidthParam.Create;
end;

function TLineWidthPEditor.GetParamType: TFigureParamClass;
begin
  Result:= TLineWidthParam;
end;

procedure TLineWidthPEditor.FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer);
begin
  CreateSpinEdit(AControl, Point(ALeft, UITop), 1, 100, GetParam.Value, @ParamChange);
end;

function TLineStylePEditor.GetParam: TLineStyleParam;
begin
  Result:= TLineStyleParam(FParam);
end;

procedure TLineStylePEditor.ParamComboBoxDrawItem(Control: TWinControl; AIndex: Integer; ARect: TRect;
                                                  AState: TOwnerDrawState);
const
  PenStyles: array[0..4] of TFPPenStyle = (psSolid, psDash, psDot, psDashDot, psDashDotDot);
begin
  if not AIndex in [Low(PenStyles)..High(PenStyles)] then Exit;
  TComboBox(Control).Canvas.Pen.Style:= PenStyles[AIndex];
  TComboBox(Control).Canvas.Line(ARect.Left+7, ARect.Bottom div 2+1, ARect.Right-7, ARect.Bottom div 2+1);
end;

procedure TLineStylePEditor.ParamChange(Sender: TObject);
var
  i: Integer;
begin
  if GetParam.Value = TFPPenStyle(TComboBox(Sender).ItemIndex) then Exit;
  GetParam.Value:= TFPPenStyle(TComboBox(Sender).ItemIndex);
  for i:= 0 to High(FParams) do
    TLineStyleParam(FParams[i]).Value:= TFPPenStyle(TComboBox(Sender).ItemIndex);
  if Assigned(FOnParamChange) then FOnParamChange;
end;

constructor TLineStylePEditor.Create;
begin
  IsFirstChange:= true;
  FParam:= TLineStyleParam.Create;
end;

function TLineStylePEditor.GetParamType: TFigureParamClass;
begin
  Result:= TLineStyleParam;
end;

procedure TLineStylePEditor.FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer);
begin
  CreateComboBox(AControl, Point(ALeft, UITop), 5, Integer(GetParam.Value), @ParamComboBoxDrawItem,
                 @ParamChange);
end;

function TShapeStylePEditor.GetParam: TShapeStyleParam;
begin
  Result:= TShapeStyleParam(FParam);
end;

procedure TShapeStylePEditor.ParamChange(Sender: TObject);
var
  i: Integer;
begin
  if GetParam.Value = TFPBrushStyle(TComboBox(Sender).ItemIndex) then Exit;
  GetParam.Value:= TFPBrushStyle(TComboBox(Sender).ItemIndex);
  for i:= 0 to High(FParams) do
    TShapeStyleParam(FParams[i]).Value:= TFPBrushStyle(TComboBox(Sender).ItemIndex);
  if Assigned(FOnParamChange) then FOnParamChange;
end;

procedure TShapeStylePEditor.ParamComboBoxDrawItem(Control: TWinControl; AIndex: Integer; ARect: TRect;
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

function TShapeStylePEditor.GetParamType: TFigureParamClass;
begin
  Result:= TShapeStyleParam;
end;

procedure TShapeStylePEditor.FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer);
begin
  CreateComboBox(AControl, Point(ALeft, UITop), 8, Integer(GetParam.Value),
                 @ParamComboBoxDrawItem, @ParamChange);
end;

constructor TShapeStylePEditor.Create;
begin
  IsFirstChange:= true;
  FParam:= TShapeStyleParam.Create;
end;

function TAngleCountPEditor.GetParam: TAngleCountParam;
begin
  Result:= TAngleCountParam(FParam);
end;

procedure TAngleCountPEditor.ParamChange(Sender: TObject);
var
  i: Integer;
begin
  if GetParam.Value = TSpinEdit(Sender).Value then Exit;
  GetParam.Value:= TSpinEdit(Sender).Value;
  for i:= 0 to High(FParams) do
    TAngleCountParam(FParams[i]).Value:= TSpinEdit(Sender).Value;
  if Assigned(FOnParamChange) then FOnParamChange;
end;

constructor TAngleCountPEditor.Create;
begin
  IsFirstChange:= true;
  FParam:= TAngleCountParam.Create;
end;

function TAngleCountPEditor.GetParamType: TFigureParamClass;
begin
  Result:= TAngleCountParam;
end;

procedure TAngleCountPEditor.FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer);
begin
  CreateSpinEdit(AControl, Point(ALeft, UITop), 3, 1000, GetParam.Value, @ParamChange);
end;

function TRoundingPEditor.GetParam: TRoundingParam;
begin
  Result:= TRoundingParam(FParam);
end;

procedure TRoundingPEditor.ParamChange(Sender: TObject);
var
  i: Integer;
begin
  if GetParam.Value = TSpinEdit(Sender).Value then Exit;
  GetParam.Value:= TSpinEdit(Sender).Value;
  for i:= 0 to High(FParams) do
    TRoundingParam(FParams[i]).Value:= TSpinEdit(Sender).Value;
  if Assigned(FOnParamChange) then FOnParamChange;
end;

constructor TRoundingPEditor.Create;
begin
  IsFirstChange:= true;
  FParam:= TRoundingParam.Create;
end;

function TRoundingPEditor.GetParamType: TFigureParamClass;
begin
  Result:= TRoundingParam;
end;

procedure TRoundingPEditor.FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer);
begin
  CreateSpinEdit(AControl, Point(ALeft, UITop), 0, 1000, GetParam.Value, @ParamChange);
end;

function CreateComboBox(AOwner: TWinControl; APoint: TPoint; AItems: array of String;
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

function CreateComboBox(AOwner: TWinControl; APoint: TPoint; AItemCount: Integer; AStartValue: Integer;
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

function CreateSpinEdit(AOwner: TWinControl; APoint: TPoint; AMin, AMax: Integer;
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

function CreateFloatSpinEdit(AOwner: TWinControl; APoint: TPoint; AMin, AMax: Double;
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

function GetFigureEditorClasses: TFigureParamEditorClassArray;
begin
  SetLength(Result, 6);
  Result[0]:= TColorPEditor;
  Result[1]:= TLineStylePEditor;
  Result[2]:= TLineWidthPEditor;
  Result[3]:= TShapeStylePEditor;
  Result[4]:= TAngleCountPEditor;
  Result[5]:= TRoundingPEditor;
end;

end.

