unit unitFigures;

{$mode objfpc}{$H+}

interface

uses
   Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
   ExtCtrls, StdCtrls, Buttons, unitScale, GraphMath;

type
  TFigureClass = class of TFigure;

  TFigure = class
    StartPoint, NextPoint: TPoint;
    StartPointW2S, NextPointW2S: TPoint;
    ButtonCaption: string;
    constructor Create(x, y: integer); virtual;
    procedure Draw(Canvas: TCanvas); virtual; abstract;
    procedure FigureMouseMove(x, y: integer); virtual;
    procedure FindTopLeftAndBottimRight(); virtual;
  end;

  TPolyline = class(TFigure)
    Line: array of TFloatPoint;
    constructor Create(x, y: integer); override;
    Procedure Draw(ACanvas: TCanvas); override;
    procedure FigureMouseMove(x, y: integer); override;
    procedure FindTopLeftAndBottimRight(); override;
  end;

  TLine = class(TFigure)
    Procedure Draw(Canvas: TCanvas); override;
  end;

  TRectangle = class(TFigure)
    Procedure Draw(Canvas: TCanvas); override;
  end;

  TEllipse = class(TFigure)
    Procedure Draw(Canvas: TCanvas); override;
  end;

var
   TopLeft, BottomRight: TPoint;

implementation

constructor TFigure.Create(x, y: integer);
begin
  StartPoint.x := x;
  StartPoint.y := y;
  StartPoint := ScreenToWorld(StartPoint);
end;

constructor TPolyLine.Create(x, y: integer);
begin
  StartPoint.x := x;
  StartPoint.y := y;
  StartPoint := ScreenToWorld(StartPoint);
  SetLength(line, length(Line) + 1);
  Line[high(Line)] := StartPoint;
end;

procedure TFigure.FigureMouseMove(x, y: integer);
begin
  NextPoint.x := x;
  NextPoint.y := y;
  NextPoint := ScreenToWorld(NextPoint);
end;

procedure TFigure.FindTopLeftAndBottimRight();
begin
  if (StartPoint.x < TopLeft.x) then TopLeft.x := StartPoint.x;
  if (NextPoint.x  < TopLeft.x) then TopLeft.x := NextPoint.x;
  if (StartPoint.y < TopLeft.y) then TopLeft.y := StartPoint.y;
  if (NextPoint.y  < TopLeft.y) then TopLeft.y := NextPoint.y;

  if (StartPoint.x > BottomRight.x) then BottomRight.x := StartPoint.x;
  if (NextPoint.x  > BottomRight.x) then BottomRight.x := NextPoint.x;
  if (StartPoint.y > BottomRight.y) then BottomRight.y := StartPoint.y;
  if (NextPoint.y  > BottomRight.y) then BottomRight.y := NextPoint.y;
end;

procedure Tpolyline.FindTopLeftAndBottimRight();
var
  p: TPoint;
begin
  for p in Line do begin
    if (p.x < TopLeft.x) then TopLeft.x := p.x;
    if (p.y < TopLeft.y) then TopLeft.y := p.y;

    if (p.x > BottomRight.x) then BottomRight.x := p.x;
    if (p.y > BottomRight.y) then BottomRight.y := p.y;
  end;
end;

procedure TPolyLine.FigureMouseMove(x, y: integer);
begin
  NextPoint.x := x;
  NextPoint.y := y;
  NextPoint := ScreenToWorld(NextPoint);
  SetLength(line, length(Line) + 1);
  Line[high(Line)] := NextPoint;
end;

Procedure TPolyLine.Draw(ACanvas: TCanvas);
var
  i: integer;
begin
  for i := 1 to high(line) do
    ACanvas.Line(WorldToScreen(Line[i-1]), WorldToScreen(Line[i]));
end;

Procedure TLine.Draw(Canvas: TCanvas);
begin
  StartPointW2S := WorldToScreen(StartPoint);
  NextPointW2S := WorldToScreen(NextPoint);
  Canvas.Line(StartPointW2S.x, StartPointW2S.y, NextPointW2S.x, NextPointW2S.y);
end;

Procedure TRectangle.Draw(Canvas: TCanvas);
begin
  Canvas.Brush.Style := bsClear;
  StartPointW2S := WorldToScreen(StartPoint);
  NextPointW2S := WorldToScreen(NextPoint);
  Canvas.Rectangle(StartPointW2S.x, StartPointW2S.y, NextPointW2S.x, NextPointW2S.y)
end;

Procedure TEllipse.Draw(Canvas: TCanvas);
begin
  Canvas.Brush.Style := bsClear;
  StartPointW2S := WorldToScreen(StartPoint);
  NextPointW2S := WorldToScreen(NextPoint);
  Canvas.Ellipse(StartPointW2S.x, StartPointW2S.y, NextPointW2S.x, NextPointW2S.y);
end;

end.

