unit UParamEditors;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, SysUtils, StdCtrls, FPCanvas, Spin, UFigureParams, Graphics;

type

  TParamChangingEvent = procedure(Sender: TObject) of object;
  TDrawItemEvent = procedure(Control: TWinControl; AIndex: Integer; ARect: TRect;
                             AState: TOwnerDrawState) of object;

  TParamEditor = class
  strict protected
  const
    UILeft: Integer = 5;
    UITop: Integer = 8;
  public
    procedure FillUserInterface(AControl: TWinControl);
    procedure FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer); virtual; abstract;
  end;

  TParamEditorArray = array of TParamEditor;

  TZoomModePEditor = class(TParamEditor)
  strict private

  public

  end;

  TZoomPowerPEditor = class(TParamEditor)
  strict private
  public
  end;

  TSelectAllPEditor = class(TParamEditor)
  strict private
    FSelectAllBtnClick: TParamChangingEvent;
  public
    property SelectAllBtnClick: TParamChangingEvent read FSelectAllBtnClick write FSelectAllBtnClick;
    procedure FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer); override;
  end;

  TColorPEditor = class(TParamEditor)
  strict private
    FParam: TColorParam;
  public
    procedure FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer); override;
  end;

  TLineWidthPEditor = class(TParamEditor)
  strict private
    FParam: TLineWidthParam;
    procedure ParamChange(Sender: TObject);
  public
    property Parameter: TLineWidthParam read FParam write FParam;
    procedure FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer); override;
  end;

  TLineStylePEditor = class(TParamEditor)
  strict private
    FParam: TLineStyleParam;
    procedure ParamComboBoxDrawItem(Control: TWinControl; AIndex: Integer; ARect: TRect;
                                    AState: TOwnerDrawState);
    procedure ParamChange(Sender: TObject);
  public
    property Parameter: TLineStyleParam read FParam write FParam;
    procedure FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer); override;
  end;

  TShapeStylePEditor = class(TParamEditor)
  strict private
    FParam: TShapeStyleParam;
    procedure ParamChange(Sender: TObject);
    procedure ParamComboBoxDrawItem(Control: TWinControl; AIndex: Integer; ARect: TRect;
                                    AState: TOwnerDrawState);
  public
    property Parameter: TShapeStyleParam read FParam write FParam;
    procedure FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer); override;

  end;

  TAngleCountPEditor = class(TParamEditor)
  strict private
    FParam: TAngleCountParam;
    procedure ParamChange(Sender: TObject);
  public
    property Parameter: TAngleCountParam read FParam write FParam;
    procedure FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer); override;
  end;

  TRoundingPEditor = class(TParamEditor)
  strict private
    FParam: TRoundingParam;
    procedure ParamChange(Sender: TObject);
  public
    property Parameter: TRoundingParam read FParam write FParam;
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

procedure TColorPEditor.FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer);
begin

end;

procedure TLineWidthPEditor.ParamChange(Sender: TObject);
begin
  FParam.Value:= TSpinEdit(Sender).Value;
end;

procedure TLineWidthPEditor.FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer);
begin
  CreateSpinEdit(AControl, Point(ALeft, UITop), 1, 100, FParam.Value, @ParamChange);
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
begin
  FParam.Value:= TFPPenStyle(TComboBox(Sender).ItemIndex);
end;

procedure TLineStylePEditor.FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer);
begin
  CreateComboBox(AControl, Point(ALeft, UITop), 5, Integer(FParam.Value), @ParamComboBoxDrawItem,
                 @ParamChange);
end;

procedure TShapeStylePEditor.ParamChange(Sender: TObject);
begin
  FParam.Value:= TFPBrushStyle(TComboBox(Sender).ItemIndex);
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

procedure TShapeStylePEditor.FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer);
begin
  CreateComboBox(AControl, Point(ALeft, UITop), 8, Integer(FParam.Value),
                 @ParamComboBoxDrawItem, @ParamChange);
end;

procedure TAngleCountPEditor.ParamChange(Sender: TObject);
begin
  FParam.Value:= TSpinEdit(Sender).Value;
end;

procedure TAngleCountPEditor.FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer);
begin
  CreateSpinEdit(AControl, Point(ALeft, UITop), 3, 1000, FParam.Value, @ParamChange);
end;

procedure TRoundingPEditor.ParamChange(Sender: TObject);
begin
  FParam.Value:= TSpinEdit(Sender).Value;
end;

procedure TRoundingPEditor.FillUserInterfaceRaw(AControl: TWinControl; var ALeft: Integer);
begin
  CreateSpinEdit(AControl, Point(ALeft, UITop), 0, 1000, FParam.Value, @ParamChange);
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



end.

