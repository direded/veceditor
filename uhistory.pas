unit UHistory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UFigures, UUtils;

type

  { THistrory }

  THistory = class
  strict private
  var
    FFigures: TFigures;
    FStates: array[0..5] of TFigureArray;
    FMinIndex, FIndex, FMaxIndex, FSavedIndex: Integer;
    FOnStateChange: TEventHandler;
    procedure IncIndex(var AValue: Integer);
    procedure DecIndex(var AValue: Integer);
    procedure IncIndex;
    procedure DecIndex;
  public
    constructor Create;
    property  OnStateChange: TEventHandler read FOnStateChange write FOnStateChange;
    function  IsSaved: Boolean;
    procedure SetFigures(AValue: TFigures);
    procedure Undo;
    procedure Redo;
    procedure AddState;
    procedure SaveState;
  end;

implementation

  { THistory }

procedure THistory.IncIndex(var AValue: Integer);
begin
  Inc(AValue);
  if AValue = High(FStates)+1 then
    AValue:= Low(FStates);
end;

procedure THistory.DecIndex(var AValue: Integer);
begin
  Dec(AValue);
  if AValue = Low(FStates)-1 then
    AValue:= High(FStates);
end;

procedure THistory.IncIndex;
begin
  IncIndex(FIndex);
end;

procedure THistory.DecIndex;
begin
  DecIndex(FIndex);
end;

constructor THistory.Create;
begin
  FSavedIndex:= -1;
  FIndex:= 0;
  FMinIndex:= 0;
  FMaxIndex:= 1;
end;

procedure THistory.SetFigures(AValue: TFigures);
begin
  FFigures:= AValue;
end;

function THistory.IsSaved: Boolean;
begin
  Result:= FIndex = FSavedIndex;
end;

procedure THistory.Undo;
begin
  if FIndex = FMinIndex then Exit;
  DecIndex;
  FFigures.SetContent(FStates[FIndex]);
  Writeln('THistrory.Undo executed'); // debug
  if Assigned(FOnStateChange) then
    FOnStateChange;
end;

procedure THistory.Redo;
begin
  IncIndex;
  if FIndex = FMaxIndex then begin
    DecIndex;
    Exit;
  end;
  FFigures.SetContent(FStates[FIndex]);
  Writeln('THistrory.Redo executed'); // debug
  if Assigned(FOnStateChange) then
    FOnStateChange;
end;

procedure THistory.SaveState;
begin
  FSavedIndex:= FIndex;
end;

procedure THistory.AddState;
begin
  IncIndex;
  if FIndex = FMinIndex then
    IncIndex(FMinIndex);
  FMaxIndex:= FIndex;
  IncIndex(FMaxIndex);
  if FIndex = FSavedIndex then
    FSavedIndex:= -1;
  FStates[FIndex]:= FFigures.CopyContent;
  Writeln('THistrory.AddState executed'); // debug
  if Assigned(FOnStateChange) then
    FOnStateChange;
end;

end.

