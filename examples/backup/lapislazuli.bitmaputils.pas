unit LapisLazuli.BitmapUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes,
  BGRAGradients, BGRAGradientScanner, BGRAResample;

function CreateSplineGradient(var AColors: array of TBGRAPixel; var APositions: array of single; APrecision: integer = 10; ASplineCoeff: single = 0.25): TBGRAMultiGradient;
function AVGColor(c1, c2: TBGRAPixel; Percent: single): TBGRAPixel;

implementation

function CreateSplineGradient(var AColors: array of TBGRAPixel; var APositions: array of single; APrecision: integer = 10; ASplineCoeff: single = 0.25): TBGRAMultiGradient;

  function ColorFToBGRA(c: TColorF): TBGRAPixel;

    function ClampByte(AValue: NativeInt): NativeInt;
    begin
      if AValue <= 0 then
        Result := 0
      else if AValue >= 255 then
        Result := 255
      else
        Result := AValue;
    end;

  begin
    Result := BGRA(ClampByte(round(c[1])), ClampByte(round(c[2])), ClampByte(round(c[3])), ClampByte(round(c[4])));
  end;

  function GetIntermediateColorF(k: TWideKernelFilter; c1, c2, c3, c4: TColorF; t: single): TColorF;
  begin
    Result := c1 * k.Interpolation(t + 1) + c2 * k.Interpolation(t) + c3 * k.Interpolation(t - 1) + c4 * k.Interpolation(t - 2);
  end;

  function GetIntermediatePosition(k: TWideKernelFilter; p1, p2, p3, p4: single; t: single): single;
  begin
    Result := p1 * k.Interpolation(t + 1) + p2 * k.Interpolation(t) + p3 * k.Interpolation(t - 1) + p4 * k.Interpolation(t - 2);
  end;

var
  paddedColorsF: array of TColorF;
  paddedPos: array of single;

  interpColors: array of TBGRAPixel;
  interpPos: array of single;

  i, j, index: integer;
  k: TWideKernelFilter;
  t: single;
begin
  if length(AColors) <> length(APositions) then
    raise EArgumentException.Create('Array size mismatch');
  if length(AColors) < 2 then
    raise EArgumentException.Create('At least 2 color needed');
  setlength(paddedColorsF, length(AColors) + 2);
  setlength(paddedPos, length(APositions) + 2);
  for i := 0 to high(AColors) do
  begin
    with AColors[i] do
      paddedColorsF[i + 1] := ColorF(red, green, blue, alpha);
    paddedPos[i + 1] := APositions[i];
  end;
  paddedColorsF[0] := paddedColorsF[1] * 2 - paddedColorsF[2];
  paddedColorsF[high(paddedColorsF)] := paddedColorsF[high(paddedColorsF) - 1] * 2 - paddedColorsF[high(paddedColorsF) - 2];
  paddedPos[0] := 2 * paddedPos[1] - paddedPos[2];
  paddedPos[high(paddedPos)] := 2 * paddedPos[high(paddedPos) - 1] - paddedPos[high(paddedPos) - 2];
  setlength(interpColors, (length(AColors) - 1) * APrecision + 1);
  setlength(interpPos, length(interpColors));
  index := 0;
  k := TSplineKernel.Create(ASplineCoeff);
  for i := 0 to high(AColors) - 1 do
  begin
    for j := 0 to APrecision - 1 do
    begin
      t := j / APrecision;
      interpPos[index] := GetIntermediatePosition(k, paddedPos[i], paddedPos[i + 1], paddedPos[i + 2], paddedPos[i + 3], t);
      interpColors[index] := ColorFToBGRA(GetIntermediateColorF(k, paddedColorsF[i], paddedColorsF[i + 1], paddedColorsF[i + 2], paddedColorsF[i + 3], t));
      Inc(index);
    end;
  end;
  k.Free;
  interpPos[index] := paddedPos[high(paddedPos) - 1];
  interpColors[index] := ColorFToBGRA(paddedColorsF[high(paddedColorsF) - 1]);
  Result := TBGRAMultiGradient.Create(interpColors, interpPos, True);
end;

function AVGColor(c1, c2: TBGRAPixel; Percent: single): TBGRAPixel;
var
  h1, h2, Res: THSLAPixel;
begin
  h1 := BGRAToHSLA(c1);
  h2 := BGRAToHSLA(c2);
  {$IFDEF INDEBUG}
  with h1 do
    WriteLn('H1', ': ', hue, ', ', saturation, ', ', lightness, ', ', alpha);
  with h2 do
    WriteLn('H2', ': ', hue, ', ', saturation, ', ', lightness, ', ', alpha);
  {$ENDIF}
  with Res do
  begin
    hue := round((h1.hue + h2.hue) * Percent / 100);
    saturation := round((h1.saturation + h2.saturation) * Percent / 100);
    lightness := round((h1.lightness + h2.lightness) * Percent / 100);
    alpha := round((h1.alpha + h2.alpha) * Percent / 100);
    {$IFDEF INDEBUG}
    WriteLn('Result: ', ',', hue, ', ', saturation, ', ', lightness, ', ', alpha);
    {$ENDIF}
  end;
  Result := HSLAToBGRA(Res);
end;

end.

