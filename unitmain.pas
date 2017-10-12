unit unitMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls;

type

  { TFormMain }

  TFormMain = class(TForm)
    MainMenu: TMainMenu;
    MenuItemFile: TMenuItem;
    MenuItemInfo: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemAbout: TMenuItem;
    PaintBoxMain: TPaintBox;
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure PaintBoxMainMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMainMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxMainMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMainPaint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses
  unitAbout;

var
  isDrawing: boolean;
  lines: array of array of TPoint;

{$R *.lfm}

{ TFormMain }

procedure TFormMain.MenuItemAboutClick(Sender: TObject);
begin
  FormAbout.Visible := True;
end;

procedure TFormMain.MenuItemExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.PaintBoxMainMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

  if (ssLeft in Shift) then begin
    isDrawing := true;
    setLength(lines, high(lines) + 2);
  end;

 PaintBoxMain.Invalidate;
end;

procedure TFormMain.PaintBoxMainMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);

var
  outPaintBox: boolean;

begin
  outPaintBox := (x > PaintBoxMain.Width) or (y > PaintBoxMain.Height) or
                 (x < PaintBoxMain.Left) or (y < PaintBoxMain.Top);

  if (isDrawing) and (not outPaintBox) then begin
    setLength(lines[high(lines)], high(lines[high(lines)]) + 2);
    lines[high(lines), high(lines[high(lines)])].x := x;
    lines[high(lines), high(lines[high(lines)])].y := y;
  end;

  if (outPaintBox) and (high(lines[high(lines)]) <> -1) then
  setLength(lines, high(lines) + 2);

  PaintBoxMain.Invalidate;
end;

procedure TFormMain.PaintBoxMainMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

begin
  isDrawing := false;
end;

procedure TFormMain.PaintBoxMainPaint(Sender: TObject);

var
  i: integer;

begin
  PaintBoxMain.Canvas.Brush.Color := clWhite;
  PaintBoxMain.Canvas.FillRect(0, 0, PaintBoxMain.Width, PaintBoxMain.Height);

  for i := 0 to high(lines) do
    FormMain.PaintBoxMain.Canvas.Polyline(lines[i]);
end;

end.

