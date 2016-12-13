unit UFigureParams;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPCanvas, Graphics;

type

  { Params }

  TFigureParam = class

  end;

  TFigureParamClass = class of TFigureParam;
  TFigureParamClassArray = array of TFigureParamClass;
  TFigureParamArray = array of TFigureParam;

  TColorParam = class(TFigureParam)
  strict private
    FValue: TColor;
  public
    property Value: TColor read FValue write FValue;
  end;

  TLineWidthParam = class(TFigureParam)
  strict private
    FValue: Integer;
  public
    property Value: Integer read FValue write FValue;
  end;

  TLineStyleParam = class(TFigureParam)
  strict private
    FValue: TFPPenStyle;
  public
    property Value: TFPPenStyle read FValue write FValue;
  end;

  TShapeStyleParam = class(TFigureParam)
  strict private
    FValue: TFPBrushStyle;
  public
    property Value: TFPBrushStyle read FValue write FValue;
  end;

  TAngleCountParam = class(TFigureParam)
  strict private
    FValue: Integer;
  public
    property Value: Integer read FValue write FValue;
  end;

  TRoundingParam = class(TFigureParam)
  strict private
    FValue: Integer;
  public
    property Value: Integer read FValue write FValue;
  end;

  { Other params }

  TZoomModeParam = class(TFigureParam)
  public
  type
    TZoomModes = (zmtlZoomIn, zmtlZoomOut, zmtlZoomSpace);
  strict private
    FValue: TZoomModes;
  public
    property Value: TZoomModes read FValue write FValue;
  end;

  TZoomPowerParam = class(TFigureParam)
  strict private
    FValue: Double;
  public
    property Value: Double read FValue write FValue;
  end;

  { Misc }

  TFigureColors = record
    Brush: TColor;
    Pen: TColor;
  end;

  TPenParams = object
  private
    FStyle: TFPPenStyle;
    FWidth: Integer;
    FColor: TColor;
  public
    property Style: TFPPenStyle read FStyle write FStyle;
    property Width: Integer read FWidth write FWidth;
    property Color: TColor read FColor write FColor;
  end;

  TBrushParams = object
  private
    FStyle: TFPBrushStyle;
    FColor: TColor;
  public
    property Style: TFPBrushStyle read FStyle write FStyle;
    property Color: TColor read FColor write FColor;
  end;

  PPenParams = ^TPenParams;
  PBrushParams = ^TBrushParams;

  procedure Push(Element: TFigureParam; Arr: TFigureParamArray);

  function MakeFigureColors(Pen, Brush: TColor): TFigureColors;

  operator =(A, B: TPenParams): Boolean;
  procedure SetCanvasParams(PenParams: TPenParams; Pen: TPen);
  function GetCanvasParams(Pen: TPen): TPenParams;

  operator =(A, B: TBrushParams): Boolean;
  procedure SetCanvasParams(BrushParams: TBrushParams; Brush: TBrush);
  function GetCanvasParams(Brush: TBrush): TBrushParams;

implementation

procedure Push(Element: TFigureParam; Arr: TFigureParamArray);
begin
  SetLength(Arr, Length(Arr)+1);
  Arr[High(Arr)]:= Element;
end;

function MakeFigureColors(Pen, Brush: TColor): TFigureColors;
begin
  Result.Pen:= Pen;
  Result.Brush:= Brush;
end;

operator =(A, B: TPenParams): Boolean;
begin
  Result:= (A.Color = B.Color) and (A.Style = B.Style) and (A.Width = B.Width);
end;

procedure SetCanvasParams(PenParams: TPenParams; Pen: TPen);
begin
  Pen.Width:= PenParams.Width;
  Pen.Style:= PenParams.Style;
  Pen.Color:= PenParams.Color;
end;

function GetCanvasParams(Pen: TPen): TPenParams;
begin
  Result.Style:= Pen.Style;
  Result.Color:= Pen.Color;
  Result.Width:= Pen.Width;
end;

operator =(A, B: TBrushParams): Boolean;
begin
  Result:= (A.Color = B.Color) and (A.Style = B.Style);
end;

procedure SetCanvasParams(BrushParams: TBrushParams; Brush: TBrush);
begin
  Brush.Style:= BrushParams.Style;
  Brush.Color:= BrushParams.Color;
end;

function GetCanvasParams(Brush: TBrush): TBrushParams;
begin
  Result.Style:= Brush.Style;
  Result.Color:= Brush.Color;
end;

end.

{
  Открыть, сохранить как, заголовок окна, измененный файл (пометка *), вопрос о сохранении данных при создании нового
  Виртуальный метод Save у фигуры, который возвращает ClassName
}

