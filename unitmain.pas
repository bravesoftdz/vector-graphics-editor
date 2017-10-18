unit unitMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, StdCtrls, Buttons, unitAbout, unitFigures, unitTools;

type

  { TFormMain }

  TFormMain = class(TForm)
    MainMenu: TMainMenu;
    MenuItemClear: TMenuItem;
    MenuItemRedo: TMenuItem;
    MenuItemUndo: TMenuItem;
    MenuItemEdit: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuItemInfo: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemAbout: TMenuItem;
    PaintBoxMain: TPaintBox;
    PanelTools: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemClearClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure MenuItemRedoClick(Sender: TObject);
    procedure MenuItemUndoClick(Sender: TObject);
    procedure PaintBoxMainMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMainMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxMainMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMainPaint(Sender: TObject);
    procedure ToolSpeedButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormMain: TFormMain;
  IsDrawing: boolean;
  Figures: array of TFigure;
  Deleted: array of TFigure;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.MenuItemExitClick(Sender: TObject);
begin
  Close();
end;

procedure TFormMain.MenuItemRedoClick(Sender: TObject);    //Wrong realization.
begin                                                      //Will be fixed
  if (high(Deleted) <> -1) then begin                      //in future.
    SetLength(Figures, length(Figures) + 1);
    Figures[high(Figures)] := Deleted[high(Deleted)];
    SetLength(Deleted, high(Deleted));
    PaintBoxMain.Invalidate();
  end;
end;

procedure TFormMain.MenuItemUndoClick(Sender: TObject);
begin
  if (high(Figures) <> -1) then begin
    SetLength(Deleted, length(Deleted) + 1);
    Deleted[high(Deleted)] := Figures[high(Figures)];
    SetLength(Figures, high(Figures));
    PaintBoxMain.Invalidate();
  end;
end;

procedure TFormMain.PaintBoxMainMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (ssLeft in Shift) then begin
    IsDrawing := true;
    SetLength(Figures, Length(Figures) + 1);
    Figures[high(Figures)] := Tool.Create(x, y);
  end;
end;

procedure TFormMain.PaintBoxMainMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if IsDrawing then begin
    Figures[high(Figures)].FigureMouseMove(x, y);
    Figures[high(Figures)].Draw(PaintBoxMain.Canvas);
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
  for Figure in Figures do Figure.Draw(PaintBoxMain.Canvas);
end;

procedure TFormMain.MenuItemAboutClick(Sender: TObject);
begin
  FormAbout.ShowModal();
end;

procedure TFormMain.MenuItemClearClick(Sender: TObject);
begin
  SetLength(Deleted, 0);
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
    b.Top     := 50 * i;
    b.Width   := PanelTools.Width;
    b.Height  := 50;
    b.OnClick := @ToolSpeedButtonClick;
    if (i = 0) then begin
      b.GroupIndex := 1;
      b.Down := True;
    end;
  end;
  Tool := TPolyline;
end;

procedure TFormMain.ToolSpeedButtonClick(Sender: TObject);
begin
  (Sender as TSpeedButton).GroupIndex := 1;
  (Sender as TSpeedButton).Down := True;
  Tool := Tools[(Sender as TSpeedButton).Tag].FigureClass;
end;

end.

