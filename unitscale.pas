unit unitScale;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GraphMath;

  procedure Scaling(AWidth, AHeight: integer; OldZoom: Double);
  function WorldToScreen(P: TFloatPoint): TPoint;
  function ScreenToWorld(P: TPoint): TFloatPoint;

var
  Zoom : integer = 100;
  Offset: TPoint;

implementation

procedure Scaling(AWidth, AHeight: integer; OldZoom: Double);
begin
  if Zoom > OldZoom then begin
      Offset.x += round(AWidth  * (Zoom - OldZoom) / 200);
      Offset.y += round(AHeight * (Zoom - OldZoom) / 200);
    end else begin
      Offset.x -= round(AWidth  * (OldZoom - Zoom) / 200);
      Offset.y -= round(AHeight * (OldZoom - Zoom) / 200);
    end;
end;

function WorldToScreen(P: TFloatPoint): TPoint;
begin
  Result.x := round(P.x * Zoom / 100) - Offset.x;
  Result.y := round(P.y * Zoom / 100) - Offset.y;
end;

function ScreenToWorld(P: TPoint): TFloatPoint;
begin
  Result.x := (P.x + Offset.x) / Zoom * 100;
  Result.y := (P.y + Offset.y) / Zoom * 100;
end;

end.

