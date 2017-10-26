unit unitMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, StdCtrls, Buttons, Spin, unitAbout, unitFigures, unitTools,
  UnitScale, Types;

type

  { TFormMain }

  TFormMain = class(TForm)
    MainMenu: TMainMenu;
    MenuItemClear: TMenuItem;
    MenuItemEdit: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuItemInfo: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemAbout: TMenuItem;
    PaintBoxMain: TPaintBox;
    PanelTools: TPanel;
    ScrollBarHorizontal: TScrollBar;
    ScrollBarVertical: TScrollBar;
    SpeedButtonHandle: TSpeedButton;
    SpinEditZoom: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemClearClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure PaintBoxMainMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMainMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxMainMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMainPaint(Sender: TObject);
    procedure ScrollBarHorizontalChange(Sender: TObject);
    procedure ScrollBarVerticalChange(Sender: TObject);
    procedure SpeedButtonNonFigureClick(Sender: TObject);
    procedure SpinEditZoomChange(Sender: TObject);
    procedure ToolSpeedButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormMain: TFormMain;
  OldOffset, StartPoint, NextPoint: TPoint;
  IsDrawing: boolean;
  Figures: array of TFigure;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.MenuItemExitClick(Sender: TObject);
begin
  Close();
end;

procedure TFormMain.PaintBoxMainMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (ssLeft in Shift) then begin
    IsDrawing := true;
    if (SpeedButtonHandle.Down) then begin
      OldOffset := Offset;
      StartPoint.x := x;
      StartPoint.y := y;
    end else begin
      SetLength(Figures, Length(Figures) + 1);
      Figures[high(Figures)] := Tool.Create(x, y);
    end;
  end;
end;

procedure TFormMain.PaintBoxMainMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if (IsDrawing) then begin
    if (SpeedButtonHandle.Down) then begin
      NextPoint.x := x;
      NextPoint.y := y;
      Offset.x := OldOffset.x + StartPoint.x - NextPoint.x;
      Offset.y := OldOffset.y + StartPoint.y - NextPoint.y;
    end else begin
      Figures[high(Figures)].FigureMouseMove(x, y);
      Figures[high(Figures)].Draw(PaintBoxMain.Canvas);
    end;
  PaintBoxMain.Invalidate();
  end;
end;

procedure TFormMain.PaintBoxMainMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = MBLeft) then begin
    IsDrawing := false;
    PaintBoxMain.Invalidate();
  end;
end;

procedure TFormMain.PaintBoxMainPaint(Sender: TObject);
var
  Figure: TFigure;
begin
  PaintBoxMain.Canvas.Brush.Color := clWhite;
  PaintBoxMain.Canvas.FillRect(0, 0, PaintBoxMain.Width, PaintBoxMain.Height);

  for Figure in Figures do begin
    Figure.Draw(PaintBoxMain.Canvas);
    Figure.FindTopLeftAndBottimRight();
  end;

  if (ScrollBarHorizontal.Max < BottomRight.x) then begin
    ScrollBarHorizontal.PageSize := ScrollBarHorizontal.PageSize +
      (- BottomRight.x + ScrollBarHorizontal.Max) div 2;
    ScrollBarHorizontal.Max := BottomRight.x;
  end;

  if (ScrollBarHorizontal.Min > TopLeft.x) then begin
    ScrollBarHorizontal.PageSize := ScrollBarHorizontal.PageSize +
      (- ScrollBarHorizontal.Min + TopLeft.x) div 2;
    ScrollBarHorizontal.Min := TopLeft.x;
  end;

  if (ScrollBarVertical.Max < BottomRight.y) then begin
    ScrollBarVertical.PageSize := ScrollBarVertical.PageSize +
      (- BottomRight.y + ScrollBarVertical.Max) div 2;
    ScrollBarVertical.Max := BottomRight.y;
  end;

  if (ScrollBarVertical.Min > TopLeft.y) then begin
    ScrollBarVertical.PageSize := ScrollBarVertical.PageSize +
      (- ScrollBarVertical.Min + TopLeft.y) div 2;
    ScrollBarVertical.Min := TopLeft.y;
  end;
end;

procedure TFormMain.ScrollBarHorizontalChange(Sender: TObject);
begin
  Offset.x := ScrollBarHorizontal.Position;
  PaintBoxMain.Invalidate;
end;

procedure TFormMain.ScrollBarVerticalChange(Sender: TObject);
begin
  Offset.y := ScrollBarVertical.Position;
  PaintBoxMain.Invalidate;
end;

procedure TFormMain.SpeedButtonNonFigureClick(Sender: TObject);
begin
  (Sender as TSpeedButton).GroupIndex := 1;
  (Sender as TSpeedButton).Down := True;
end;

procedure TFormMain.SpinEditZoomChange(Sender: TObject);
var
  OldZoom: double;
begin
  OldZoom := Zoom;
  Zoom := SpinEditZoom.Value;
  Scaling(PaintBoxMain.Width, PaintBoxMain.Height, OldZoom);
  PaintBoxMain.Invalidate;
end;

procedure TFormMain.MenuItemAboutClick(Sender: TObject);
begin
  FormAbout.ShowModal();
end;

procedure TFormMain.MenuItemClearClick(Sender: TObject);
begin
  SetLength(Figures, 0);
  PaintBoxMain.Invalidate;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  i: integer;
  b: TSpeedButton;
begin
  for i := 0 to high(Tools) do
  begin
    b         := TSpeedButton.Create(FormMain);
    b.Parent  := PanelTools;
    b.Caption := Tools[i].ButtonCaption;;
    b.Tag     := i;
    b.Left    := 0;
    b.Top     := 50 * (i + 1);
    b.Width   := PanelTools.Width;
    b.Height  := 50;
    b.OnClick := @ToolSpeedButtonClick;
    if (i = 0) then begin
      b.GroupIndex := 1;
      b.Down := True;
    end;
  end;

  SpinEditZoom.Top := 30 + 50 * (i + 2);
  Tool := Tools[0].FigureClass;
  BottomRight.x := PaintBoxMain.Width;
  BottomRight.y := PaintBoxMain.Height;
end;

procedure TFormMain.ToolSpeedButtonClick(Sender: TObject);
begin
  (Sender as TSpeedButton).GroupIndex := 1;
  (Sender as TSpeedButton).Down := True;
  Tool := Tools[(Sender as TSpeedButton).Tag].FigureClass;
end;

end.

