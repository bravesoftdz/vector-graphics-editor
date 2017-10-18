unit unitFigures;

{$mode objfpc}{$H+}

interface

uses
   Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
   ExtCtrls, StdCtrls, Buttons;

type
  TFigureClass = class of TFigure;

  TFigure = class
    StartPoint, NextPoint: TPoint;
    ButtonCaption: string;
    constructor Create(x, y: integer); virtual;
    procedure Draw(Canvas: TCanvas); virtual; abstract;
    procedure FigureMouseMove(x, y: integer); virtual;
  end;

  TPolyline = class(TFigure)
    Line: array of TPoint;
    constructor Create(x, y: integer); override;
    Procedure Draw(Canvas: TCanvas); override;
    procedure FigureMouseMove(x, y: integer); override;
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

implementation

constructor TFigure.Create(x, y: integer);
begin
  StartPoint.x := x;
  StartPoint.y := y;
end;

procedure TFigure.FigureMouseMove(x, y: integer);
begin
  NextPoint.x := x;
  NextPoint.y := y;
end;

constructor TPolyLine.Create(x, y: integer);
begin
  StartPoint.x := x;
  StartPoint.y := y;
  SetLength(line, length(Line) + 1);
  Line[high(Line)] := StartPoint;
end;

procedure TPolyLine.FigureMouseMove(x, y: integer);
begin
  NextPoint.x := x;
  NextPoint.y := y;
  SetLength(line, length(Line) + 1);
  Line[high(Line)] := NextPoint;
end;

Procedure TPolyLine.Draw(Canvas: TCanvas);
begin
  Canvas.PolyLine(Line);
end;

Procedure TLine.Draw(Canvas: TCanvas);
begin
  Canvas.Line(StartPoint.x, StartPoint.y, NextPoint.x, NextPoint.y);
end;

Procedure TRectangle.Draw(Canvas: TCanvas);
begin
  Canvas.Rectangle(StartPoint.x, StartPoint.y, NextPoint.x, NextPoint.y)
end;

Procedure TEllipse.Draw(Canvas: TCanvas);
begin
  Canvas.Ellipse(StartPoint.x, StartPoint.y, NextPoint.x, NextPoint.y);
end;

end.
