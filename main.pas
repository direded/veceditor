unit Main;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, Buttons, About, UTools, UFigures, Grids, StdCtrls, Spin,
  UPaintSpace, UDoublePoint, Math, Types, UFigureParams, LCLType, strutils, UHistory;

type

  { TMainForm }
  TMainForm = class(TForm)
  published
    BottomPanel: TPanel;
    EditSubMenu: TMenuItem;
    UndoItem: TMenuItem;
    RedoItem: TMenuItem;
    SaveDialog: TSaveDialog;
    SaveItem: TMenuItem;
    SaveAsItem: TMenuItem;
    Load: TMenuItem;
    PaintSpacePanel: TPanel;
    ScaleFullExtentBtn: TButton;
    ColorBarGrid: TDrawGrid;
    ColorPaletteSubmenu: TMenuItem;
    DefaultColorPaletteItem: TMenuItem;
    AddColorColorPaletteItem: TMenuItem;
    MousePosLabel: TLabel;
    ScaleSpinEdit: TFloatSpinEdit;
    HoriPaintSpaceScrl: TScrollBar;
    PenColorPanel: TPanel;
    BrushColorPanel: TPanel;
    ScrlsCrossingPanel: TPanel;
    SelectColorDialog: TColorDialog;
    MainMenu: TMainMenu;
    FileSubmenu: TMenuItem;
    HelpSubmenu: TMenuItem;
    ExitItem: TMenuItem;
    AboutItem: TMenuItem;
    MainPanel: TPanel;
    DelAllObjMenuItem: TMenuItem;
    ObjManagerSubmenu: TMenuItem;
    DelLastObjItem: TMenuItem;
    PaintSpacePaintBox: TPaintBox;
    ColorBarPanel: TPanel;
    CurrentColorsPanel: TPanel;
    ToolParamsPanel: TPanel;
    ToolsPanel: TPanel;
    VertPaintSpaceScrl: TScrollBar;
    procedure AddColorColorPaletteItemClick(Sender: TObject);
    procedure BrushColorPanelDblClick(Sender: TObject);
    procedure ColorBarGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure ColorBarGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DefaultColorPaletteItemClick(Sender: TObject);
    procedure DelAllObjMenuItemClick(Sender: TObject);
    procedure AboutItemClick(Sender: TObject);
    procedure ExitItemClick(Sender: TObject);
    procedure HoriPaintSpaceScrlChange(Sender: TObject);
    procedure LoadClick(Sender: TObject);
    procedure MainFormKeyPress(Sender: TObject; var Key: char);
    procedure MainFormCreate(Sender: TObject);
    procedure PaintSpacePaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PaintSpacePaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure PaintSpacePaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PaintSpacePaintBoxMouseWheelDown(Sender: TObject;
      Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure PaintSpacePaintBoxMouseWheelUp(Sender: TObject;
      Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure PaintSpacePaintBoxPaint(Sender: TObject);
    procedure PaintSpacePaintBoxResize(Sender: TObject);
    procedure PenColorPanelDblClick(Sender: TObject);
    procedure RedoItemClick(Sender: TObject);
    procedure SaveAsItemClick(Sender: TObject);
    procedure SaveItemClick(Sender: TObject);
    procedure ScaleFullExtentBtnClick(Sender: TObject);
    procedure ScaleSpinChange(Sender: TObject);
    procedure ToolBtnClick(Sender: TObject);
    procedure DelLastObjItemClick(Sender: TObject);
    procedure UndoItemClick(Sender: TObject);
    procedure VertPaintSpaceScrlChange(Sender: TObject);
    procedure PaintSpaceScaleChange;
    procedure PaintSpacePositionChange;
    procedure FiguresFigureAdd;
    procedure ToolParamsListChange;
    procedure SaveFile(AFilePath: String);
    procedure UpdateFormTitle;
    procedure FigureAdded;
  end;

	TColorArray = array of TColor;
  procedure UpdateVertScroll(AScrollBar: TScrollBar; APaintSpace: TPaintSpace);
  procedure UpdateHoriScroll(AScrollBar: TScrollBar; APaintSpace: TPaintSpace);
  procedure UpdateScaleSpin(AScaleSpin: TFloatSpinEdit; APaintSpace: TPaintSpace);
  function MakeDefaultPalette: TColorArray;

var
  MainForm: TMainForm;
  CurrentTool: record
    Tool: TTool;
    FigureColors: TFigureColors;
    State: (ctsReady, ctsInUse);
  end;
  PaintSpace: TPaintSpace;
  Figures: TFigures;
  ColorPalette: TColorArray;
  FileName: String;
  FilePath: String;
  History: THistory;

const
  VecEditorName: String = 'Graphic editor';

implementation

{$R *.lfm}

procedure TMainForm.ExitItemClick(Sender: TObject);
begin
  MainForm.Close;
end;

procedure TMainForm.HoriPaintSpaceScrlChange(Sender: TObject);
begin
  PaintSpace.Position:= (GetDoublePoint(HoriPaintSpaceScrl.Position, PaintSpace.Position.Y));
end;

procedure TMainForm.LoadClick(Sender: TObject);
var
  FileToLoad: TextFile;
  Answer: Integer;
  OpenDialog: TOpenDialog;
begin
  Answer:= Application.MessageBox('Do you want to save current file?', '', MB_ICONQUESTION + MB_YESNOCANCEL);
  if Answer = IDYES then
    SaveFile(FileName)
  else if Answer = IDCANCEL then Exit
  else if Answer = IDCLOSE then Exit;
  OpenDialog:= TOpenDialog.Create(nil);
  OpenDialog.InitialDir:= GetCurrentDir;
  OpenDialog.Title:= 'Open';
  OpenDialog.DefaultExt:= 'vce';
  OpenDialog.Filter:= '|*.vce|';
  OpenDialog.FileName:= FileName;
  if OpenDialog.Execute then begin
    if FileExists(OpenDialog.FileName) then begin
      AssignFile(FileToLoad, OpenDialog.FileName);
      Reset(FileToLoad);
      Figures.Load(FileToLoad);
      CloseFile(FileToLoad);
    end;
  end;
end;

procedure TMainForm.MainFormKeyPress(Sender: TObject; var Key: char);
begin
 if Key = '1' then History.AddState;
end;

procedure TMainForm.MainFormCreate(Sender: TObject);
var
  i: Integer;
  ToolBtn: TBitBtn;
begin
  UpdateFormTitle;
  PaintSpace:= TPaintSpace.Create(PaintSpacePaintBox, GetDoublePoint(1600, 900));
  PaintSpace.OnScaleChange:= @PaintSpaceScaleChange;
  PaintSpace.OnPositionChange:= @PaintSpacePositionChange;
  Figures.OnFigureAdd:= @MainForm.FiguresFigureAdd;
  ScaleSpinEdit.MaxValue:= PaintSpace.MAX_SCALE*100;
  PaintSpace.Scale:= 1;
  PaintSpace.Position:= GetDoublePoint;
  History.OnStateChange:= @MainForm.UpdateFormTitle;
  for i:= Low(Tools) to High(Tools) do begin
  	ToolBtn:= TBitBtn.Create(nil);
    ToolBtn.Parent:= ToolsPanel;
    ToolBtn.Tag:= i;
    ToolBtn.Width:= 32; ToolBtn.Height:= 32;
    ToolBtn.Left:= 0; ToolBtn.Top:= (i-Low(Tools))*32;
    ToolBtn.Glyph:= Tools[i].Metadata.Bitmap;
    ToolBtn.OnClick:= @ToolBtnClick;
    Tools[i].SetParamColor(CurrentTool.FigureColors);
    Tools[i].PaintSpace:= PaintSpace;
    Tools[i].Figures:= Figures;
    Tools[i].OnParamsListChange:= @ToolParamsListChange;
    Tools[i].OnHistoryChange:= @(History.AddState);
  end;
  PenColorPanel.Top:= ToolBtn.Top + ToolBtn.Height + PenColorPanel.Top;
  PenColorPanel.Color:= CurrentTool.FigureColors.Pen;
  BrushColorPanel.Top:= ToolBtn.Top + ToolBtn.Height + BrushColorPanel.Top;
  BrushColorPanel.Color:= CurrentTool.FigureColors.Brush;
  ColorBarGrid.RowCount:= Length(ColorPalette);
  CurrentTool.Tool.SetParamsPanel(ToolParamsPanel);
end;

procedure TMainForm.PaintSpacePaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  p: TPoint;
begin
  if CurrentTool.State = ctsInUse then Exit;
  p:= Point(X,Y);
  CurrentTool.Tool.MouseDown(PaintSpace.ToWorld(p), Shift);
	CurrentTool.State:= ctsInUse;
end;

procedure TMainForm.PaintSpacePaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
var
  p: TPoint;
begin
  p:= Point(X, Y);
  MousePosLabel.Caption:= PaintSpace.ToWorld(p).ToString;
  if CurrentTool.State = ctsReady then Exit;
  if not (ssLeft in Shift) then exit;
  CurrentTool.Tool.MouseMove(PaintSpace.ToWorld(p), Shift);
  PaintSpacePaintBox.Invalidate;
end;

procedure TMainForm.PaintSpacePaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  p: TPoint;
begin
  if CurrentTool.State = ctsReady then Exit;
  p:= Point(X,Y);
  CurrentTool.Tool.MouseUp(PaintSpace.ToWorld(p), Shift);
  PaintSpacePaintBox.Invalidate;
  CurrentTool.State:= ctsReady;
end;

procedure TMainForm.PaintSpacePaintBoxMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
end;

procedure TMainForm.PaintSpacePaintBoxMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
end;

procedure TMainForm.PaintSpacePaintBoxPaint(Sender: TObject);
begin
  Figures.Draw(PaintSpace);
end;

procedure TMainForm.PaintSpacePaintBoxResize(Sender: TObject);
begin
  PaintSpacePositionChange;
end;

procedure TMainForm.PenColorPanelDblClick(Sender: TObject);
begin
	if not SelectColorDialog.Execute then Exit;
  CurrentTool.FigureColors.Pen:= SelectColorDialog.Color;
	CurrentTool.Tool.SetParamColor(CurrentTool.FigureColors);
	PenColorPanel.Color:= CurrentTool.FigureColors.Pen;
end;

procedure TMainForm.RedoItemClick(Sender: TObject);
begin
  History.Redo;
end;

procedure TMainForm.SaveAsItemClick(Sender: TObject);
var
  i, j: Integer;
begin
  SaveDialog.InitialDir:= GetCurrentDir;
  SaveDialog.Title:= 'Save As';
  SaveDialog.DefaultExt:= 'vce';
  SaveDialog.Filter:= '|*.vce|';
  SaveDialog.FileName:= FileName;
  if SaveDialog.Execute then begin
    if FileExists(SaveDialog.FileName) then begin
      if (Application.MessageBox('File exist. Overwrite?', '', MB_ICONQUESTION + MB_YESNO) = IDYES) then begin
        SaveFile(SaveDialog.FileName);
      end else begin
        SaveAsItem.Click;
        Exit;
      end;
    end else begin
      SaveFile(SaveDialog.FileName);
    end;
  end;
end;

procedure TMainForm.SaveItemClick(Sender: TObject);
begin
  SaveFile(FileName);
end;

procedure TMainForm.ScaleFullExtentBtnClick(Sender: TObject);
var
  Min, Max: TDoublePoint;
begin
  Figures.GetBounds(Min, Max);
  if Min = Max then exit;
  if (Max.X-Min.X) > PaintSpacePaintBox.Width*(Max.Y-Min.Y)/PaintSpacePaintBox.Height then
    PaintSpace.Scale:= PaintSpace.PaintBox.Width/(Max.X-Min.X)
  else
    PaintSpace.Scale:= PaintSpace.PaintBox.Height/(Max.Y-Min.Y);
  PaintSpace.Position:= GetDoublePoint(Min.X, Min.Y);
end;

procedure TMainForm.ScaleSpinChange(Sender: TObject);
begin
	PaintSpace.SetScaleCenter(ScaleSpinEdit.Value / 100);
end;

procedure TMainForm.ToolBtnClick(Sender: TObject);
begin
  CurrentTool.Tool.CleanUp;
  CurrentTool.Tool:= Tools[(Sender as TBitBtn).Tag];
  CurrentTool.Tool.SetParamsPanel(ToolParamsPanel);
  CurrentTool.Tool.SetParamColor(CurrentTool.FigureColors);
end;

procedure TMainForm.DelLastObjItemClick(Sender: TObject);
begin
  Figures.RemoveLastFigure;
  PaintSpacePaintBox.Invalidate;
end;

procedure TMainForm.UndoItemClick(Sender: TObject);
begin
  History.Undo;
end;

procedure TMainForm.VertPaintSpaceScrlChange(Sender: TObject);
begin
  PaintSpace.Position:= GetDoublePoint(PaintSpace.Position.X, VertPaintSpaceScrl.Position);
end;

procedure TMainForm.AboutItemClick(Sender: TObject);
begin
  aboutForm.ShowModal;
end;

procedure TMainForm.DelAllObjMenuItemClick(Sender: TObject);
begin
  Figures.Free;
  Figures := TFigures.Create;
  PaintSpacePaintBox.Invalidate;
end;

procedure TMainForm.ColorBarGridDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
const ROW_H: Integer = 24;
begin
  with ColorBarGrid do begin
    RowCount:= Length(ColorPalette);
  	Canvas.Brush.Color:= ColorPalette[ARow];
  	Canvas.FillRect(CellRect(0, ARow));
	end;
end;

procedure TMainForm.AddColorColorPaletteItemClick(Sender: TObject);
begin
  SetLength(ColorPalette, Length(ColorPalette)+1);
  ColorPalette[High(ColorPalette)]:= $ffffff;
  ColorBarGrid.Invalidate;
end;

procedure TMainForm.BrushColorPanelDblClick(Sender: TObject);
begin
  if not SelectColorDialog.Execute then Exit;
  CurrentTool.FigureColors.Brush:= SelectColorDialog.Color;
	CurrentTool.Tool.SetParamColor(CurrentTool.FigureColors);
	BrushColorPanel.Color:= CurrentTool.FigureColors.Brush;
end;

procedure TMainForm.ColorBarGridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Col, Row: Integer;
begin
  ColorBarGrid.MouseToCell(X, Y, Col, Row);
  if ssDouble in Shift then
    SelectColorDialog.Color:= ColorPalette[Row];
  if (ssDouble in Shift) and SelectColorDialog.Execute then
  	ColorPalette[Row]:= SelectColorDialog.Color;
  if ssLeft in Shift then
  	CurrentTool.FigureColors.Pen:= ColorPalette[Row];
  if ssRight in Shift then
		CurrentTool.FigureColors.Brush:= ColorPalette[Row];
  CurrentTool.Tool.SetParamColor(CurrentTool.FigureColors);
  PenColorPanel.Color:= CurrentTool.FigureColors.Pen;
  BrushColorPanel.Color:= CurrentTool.FigureColors.Brush;
end;

procedure TMainForm.DefaultColorPaletteItemClick(Sender: TObject);
begin
	ColorPalette:= MakeDefaultPalette;
  ColorBarGrid.Invalidate;
end;

procedure TMainForm.PaintSpacePositionChange;
begin
  UpdateVertScroll(VertPaintSpaceScrl, PaintSpace);
  UpdateHoriScroll(HoriPaintSpaceScrl, PaintSpace);
  PaintSpacePaintBox.Invalidate;
end;

procedure TMainForm.FiguresFigureAdd;
begin
  UpdateVertScroll(VertPaintSpaceScrl, PaintSpace);
  UpdateHoriScroll(HoriPaintSpaceScrl, PaintSpace);
  PaintSpacePaintBox.Invalidate;
end;

procedure TMainForm.ToolParamsListChange;
begin
  CurrentTool.Tool.SetParamsPanel(ToolParamsPanel);
end;

procedure TMainForm.SaveFile(AFilePath: String);
var
  FileToSave: TextFile;
  i: Integer;
begin
  AssignFile(FileToSave, AFilePath);
  Rewrite(FileToSave);
  WriteLn(FileToSave, '^VCE');
  Figures.Save(FileToSave);
  for i:= High(AFilePath) downto Low(AFilePath) do
    if AFilePath[i] = '/' then Break;
  if Pos('/', AFilePath) <> 0 then
    FileName:= RightStr(AFilePath, Length(AFilePath)-i);
  FilePath:= AFilePath;
  UpdateFormTitle;
  CloseFile(FileToSave);
  History.SaveState;
  UpdateFormTitle;
end;

procedure TMainForm.UpdateFormTitle;
begin
  MainForm.Caption:= VecEditorName;
  if FileName <> '' then
    MainForm.Caption:= MainForm.Caption+' - '+FileName;
  if not History.IsSaved then
    MainForm.Caption:= MainForm.Caption+' *';
end;

procedure TMainForm.FigureAdded;
begin
  PaintSpacePaintBox.Invalidate;
end;

procedure TMainForm.PaintSpaceScaleChange;
begin
  UpdateScaleSpin(ScaleSpinEdit, PaintSpace);
  PaintSpacePositionChange;
end;

function MakeDefaultPalette: TColorArray;
const c: array [1..15] of TColor = (
	$444EF1, $5EBDFF, $84FEFF, $68F168, $E9CC8D, $EC6868,
  $E355B4, $ffffff, $dddddd, $bbbbbb, $999999, $777777,
  $555555, $333333, $000000);
var
  i: Integer;
begin
  SetLength(Result, Length(c));
  for i:= 0 to High(Result) do
    Result[i]:= c[i+1];
end;

procedure UpdateScaleSpin(AScaleSpin: TFloatSpinEdit; APaintSpace: TPaintSpace);
begin
  AScaleSpin.Value:= APaintSpace.Scale * 100;
end;

procedure UpdateVertScroll(AScrollBar: TScrollBar; APaintSpace: TPaintSpace);
begin
  AScrollBar.Min:= round(APaintSpace.WorldSpace.TopLeft.Y);
  if APaintSpace.WorldSpace.TopLeft.Y+APaintSpace.WorldSpace.Height-APaintSpace.LocalSpace.Height >= 0 then
    AScrollBar.Max:= round(APaintSpace.WorldSpace.TopLeft.Y+APaintSpace.WorldSpace.Height-APaintSpace.LocalSpace.Height);
  AScrollBar.PageSize:= round(APaintSpace.LocalSpace.Height);
  AScrollBar.Position:= round(APaintSpace.Position.Y);
end;

procedure UpdateHoriScroll(AScrollBar: TScrollBar; APaintSpace: TPaintSpace);
begin
  AScrollBar.Min:= round(APaintSpace.WorldSpace.TopLeft.X);
  if APaintSpace.WorldSpace.TopLeft.X+APaintSpace.WorldSpace.Width-APaintSpace.LocalSpace.Width >= 0 then
    AScrollBar.Max:= round(APaintSpace.WorldSpace.TopLeft.X+APaintSpace.WorldSpace.Width-APaintSpace.LocalSpace.Width);
  AScrollBar.PageSize:= round(APaintSpace.LocalSpace.Width);
  AScrollBar.Position:= round(APaintSpace.Position.X);
end;

initialization
  FileName:= 'untitled.vce';
	Figures:= TFigures.Create;
  History:= THistory.Create;
  History.SetFigures(Figures);
  CurrentTool.Tool:= Tools[0];
  CurrentTool.FigureColors:= MakeFigureColors(clBlack, clWhite);
  CurrentTool.State:= ctsReady;
  ColorPalette:= MakeDefaultPalette;
end.
