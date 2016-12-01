unit UFigureParams;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPCanvas, Graphics;

type

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

  function MakeFigureColors(Pen, Brush: TColor): TFigureColors;

  operator =(A, B: TPenParams): Boolean;
  procedure SetParams(PenParams: TPenParams; Pen: TPen);
  function GetParams(Pen: TPen): TPenParams;

  operator =(A, B: TBrushParams): Boolean;
  procedure SetParams(BrushParams: TBrushParams; Brush: TBrush);
  function GetParams(Brush: TBrush): TBrushParams;

implementation

function MakeFigureColors(Pen, Brush: TColor): TFigureColors;
begin
  Result.Pen:= Pen;
  Result.Brush:= Brush;
end;

operator =(A, B: TPenParams): Boolean;
begin
  Result:= (A.Color = B.Color) and (A.Style = B.Style) and (A.Width = B.Width);
end;

procedure SetParams(PenParams: TPenParams; Pen: TPen);
begin
  Pen.Width:= PenParams.Width;
  Pen.Style:= PenParams.Style;
  Pen.Color:= PenParams.Color;
end;

function GetParams(Pen: TPen): TPenParams;
begin
  Result.Style:= Pen.Style;
  Result.Color:= Pen.Color;
  Result.Width:= Pen.Width;
end;

operator =(A, B: TBrushParams): Boolean;
begin
  Result:= (A.Color = B.Color) and (A.Style = B.Style);
end;

procedure SetParams(BrushParams: TBrushParams; Brush: TBrush);
begin
  Brush.Style:= BrushParams.Style;
  Brush.Color:= BrushParams.Color;
end;

function GetParams(Brush: TBrush): TBrushParams;
begin
  Result.Style:= Brush.Style;
  Result.Color:= Brush.Color;
end;

end.

