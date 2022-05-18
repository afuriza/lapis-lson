unit LapisLazuli.BaseCtrls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, BGRABitmap, BGRABitmapTypes, Math,
  BGRAGradients, BGRAGradientScanner, BGRAResample, BGRATextFX,
  LCLType, Graphics, Controls, TextStrings,
  LapisLazuli.BitmapUtils,
  LapisLazuli.Types;

type

  TBaseLCControl = class(TCustomControl)
  private
    {$IFDEF INDEBUG}
    FPaintCount: Integer;
    {$ENDIF}
    FUpdateCount: Integer;
  protected
    procedure DoOnResize; override;
  protected
    {$IFDEF INDEBUG}
    function GetDebugText: String; virtual;
    {$ENDIF}
    procedure Paint; override; // do not override in descendants!
    // All descendants should use DrawControl method instead of Paint.
    // DrawControl is not called between BeginUpdate and EndUpdate
    procedure DrawControl; virtual;
    // This method is called when control should be rendered (when some
    // general action occur which change "body" e.g. resize)
    procedure RenderControl; virtual;
  public
    {$IFDEF FPC}
    { Save all published settings to file }
    procedure SaveToFile(AFileName: string); virtual; abstract;
    { Load and assign all published settings from file }
    procedure LoadFromFile(AFileName: string); virtual; abstract;
    { Assign the properties from AFileName to this instance }
    procedure AssignFromFile(AFileName: string); virtual; abstract;
    { Used by SaveToFile/LoadFromFile }
    {$ENDIF}
    constructor Create(AOwner: TComponent); override;
    // This disable DrawControl method
    procedure BeginUpdate; virtual;
    // This enable DrawControl method
    procedure EndUpdate; virtual;
    // Called on EndUpdate if FUpdateCount is 0
    procedure UpdateControl; virtual;
    // Check if BeginUpdate was called
    function IsUpdating: Boolean;
  end;

  TLCStyle = class(TLCPropertyAttrs)
  private
    fGradientColors: TStrings;
    fFont: TFont;
    fParentFont: boolean;
    fRounding: TLCRounding;
    procedure SetGradientColors(AValue: TStrings);
    procedure SetFont(Value: TFont);
    procedure SetRounding(AValue: TLCRounding);
    procedure FontChanged(Sender: TObject);
    function IsFontStored: Boolean;
  public
    constructor Create(AControl: TControl); override;
    destructor Destroy; override;
  published
    property GradientColors: TStrings read fGradientColors write SetGradientColors;
    property Font: TFont read fFont write SetFont stored IsFontStored;
    property Rounding: TLCRounding read fRounding write setRounding;
  end;

  TCustomLCControl = class(TBaseLCControl)
  private
    fUseStyle: Boolean;
    fStyle: TLCStyle;
    procedure SetUseStyle(AValue: boolean);
    procedure SetStyle(AValue: TLCStyle);
    function ParseColorList(var Colors: TStrings): TBGRAPixelBuffer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DrawControl; override;
    procedure DrawCaption(var ABitmap: TBGRABitmap; var AStyle: TLCStyle);
    procedure DrawBackground(var ABitmap: TBGRABitmap; var AStyle: TLCStyle);
  published
    // should be use LSON Style
    property UseStyle: boolean read fUseStyle write setUseStyle default True;
    property Style: TLCStyle read fStyle write setStyle;
  end;

implementation



{ TBaseLCControl }

procedure TBaseLCControl.DoOnResize;
begin
  inherited DoOnResize;
  RenderControl;
end;

{$IFDEF INDEBUG}
function TBaseLCControl.GetDebugText: String;
begin
  Result := EmptyStr;
end;
{$ENDIF}

procedure TBaseLCControl.Paint;
begin
  //inherited Paint;
  if (csCreating in ControlState) or IsUpdating then
    Exit;
  DrawControl;
  {$IFDEF INDEBUG}
  FPaintCount := FPaintCount +1;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := clBlack;
  Canvas.Font.Color := clWhite;
  Canvas.TextOut(1,1,'P: '+IntToStr(FPaintCount)+' '+GetDebugText);
  {$ENDIF}
end;

procedure TBaseLCControl.DrawControl;
begin

end;

procedure TBaseLCControl.RenderControl;
begin

end;

constructor TBaseLCControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF INDEBUG}
  FPaintCount := 0;
  {$ENDIF}
end;

procedure TBaseLCControl.BeginUpdate;
begin
  FUpdateCount := FUpdateCount +1;
end;

procedure TBaseLCControl.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    FUpdateCount := FUpdateCount -1;
    if FUpdateCount=0 then
      UpdateControl;
  end;
end;

procedure TBaseLCControl.UpdateControl;
begin
  Invalidate;
end;

function TBaseLCControl.IsUpdating: Boolean;
begin
  Result := FUpdateCount>0;
end;

{ TLCStyle }

procedure TLCStyle.SetFont(Value: TFont);
begin
  if FFont.IsEqual(Value) then exit;
  FFont.Assign(Value);
  Change;
end;

function TLCStyle.IsFontStored: Boolean;
begin
  if Assigned(fControl.Parent) then
  begin
    if fParentFont then
    begin
      Font := Control.Parent.Font;
      FParentFont := True;
    end;
  end;
  Result := not fParentFont;
end;

procedure TLCStyle.FontChanged(Sender: TObject);
begin
  FParentFont := False;
  Control.Perform(CM_FONTCHANGED, 0, 0);
  if fControl.AutoSize then
  begin
    fControl.InvalidatePreferredSize;
    fControl.AdjustSize;
  end;
  Change;
end;

procedure TLCStyle.SetRounding(AValue: TLCRounding);
begin
  if fRounding = AValue then
    Exit;
  fRounding.Assign(AValue);

  Change;
end;

constructor TLCStyle.Create(AControl: TControl);
begin
  inherited Create(AControl);
  fGradientColors := TTextStrings.Create;
  TTextStrings(fGradientColors).OnChange := @FontChanged;
  FFont := TFont.Create;
  FFont.OnChange := @FontChanged;
  FParentFont := fControl.IsParentFont;
  fRounding := TLCRounding.Create(Control);

end;

destructor TLCStyle.Destroy;
begin
  FreeAndNil(fGradientColors);
  FreeAndNil(fFont);
  freeAndNil(fRounding);
  inherited Destroy;
end;

procedure TLCStyle.SetGradientColors(AValue: TStrings);
begin
  if fGradientColors = AValue then
    Exit;
  fGradientColors.Assign(AValue);

  Change;
end;

{ TCustomLCControl }

constructor TCustomLCControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fStyle := TLCStyle.Create(Self);
  fUseStyle := UseStyle;
end;

destructor TCustomLCControl.Destroy;
begin
  freeAndNil(fStyle);
  inherited Destroy;
end;

function TCustomLCControl.ParseColorList(var Colors: TStrings): TBGRAPixelBuffer;
var
  i: integer;
begin
  SetLength(Result, Colors.Count);
  for i := 0 to Colors.Count -1 do
  begin
    Result[i] := ColorToBGRA(StringToColor(Colors[i]));
  end;
end;

procedure TCustomLCControl.SetUseStyle(AValue: boolean);
begin
  fUseStyle := AValue;

  RenderControl;
  Invalidate;
end;

procedure TCustomLCControl.SetStyle(AValue: TLCStyle);
begin
  if fStyle = AValue then
    Exit;
  fStyle.Assign(AValue);

  RenderControl;
  Invalidate;
end;

procedure TCustomLCControl.DrawCaption(var ABitmap: TBGRABitmap; var AStyle: TLCStyle);
var
  Renderer: TBGRATextEffectFontRenderer;
begin
  Renderer := TBGRATextEffectFontRenderer.Create;
  ABitmap.FontRenderer := Renderer;
  ABitmap.FontQuality := fqFineAntialiasing;


  ABitmap.FontName := AStyle.Font.Name;
  if AStyle.Font.Height = 0 then
    ABitmap.FontHeight := MulDiv(abs(GetFontData(Font.Reference.Handle).Height)-2,
      Font.PixelsPerInch, Screen.PixelsPerInch)
  else
    ABitmap.FontHeight := MulDiv(AStyle.Font.Height-2, AStyle.Font.PixelsPerInch,
    Screen.PixelsPerInch);
  ABitmap.FontStyle := AStyle.Font.Style;
  //  todo: make aligment property
  ABitmap.TextOut(Width div 2, (Height-ABitmap.FontFullHeight) div 2, Caption,
    ColorToBGRA(AStyle.Font.Color), taCenter);

  //ABitmap.TextOut(5, 5, Caption, CSSWhite, taLeftJustify);
end;

procedure TCustomLCControl.DrawBackground(var ABitmap: TBGRABitmap; var AStyle: TLCStyle);
var
  RoundX, RoundY: integer;
  p1, p2: TPointF;
  g: TBGRAMultiGradient;
  gs: TBGRAGradientScanner;
  function BGRAGradient: TBGRAMultiGradient;
  var
    IncrementedSegment: single;
    GradientSegment: array of single;
    i: integer;
    Colors: TStrings;
    BGRAPixels: array of TBGRAPixel;
  begin
    SetLength(GradientSegment, AStyle.GradientColors.Count);
    IncrementedSegment := 0;
    for i := 0 to AStyle.GradientColors.Count -1 do
    begin
      GradientSegment[i] := incrementedSegment;
      IncrementedSegment += 1 / (AStyle.GradientColors.Count -1);
    end;
    Colors := AStyle.GradientColors;
    BGRAPixels := ParseColorList(Colors);
    Result := CreateSplineGradient(BGRAPixels, GradientSegment);
  end;

begin
  RoundX := AStyle.Rounding.RoundX;
  RoundY := AStyle.Rounding.RoundY;

  { draw gradient }
  p1 := PointF(0, 0);
  p2 := PointF(0, Height);

  if AStyle.GradientColors.Count > 1 then
  begin
    g := BGRAGradient;
    //g := CreateSplineGradient([BGRA(0, 255, 0), BGRA(0, 0, 255)], [0, 1]);
    //g := CreateSplineGradient([BGRA(0, 255, 0), AVGColor(BGRA(0, 255, 0), BGRA(0, 0, 255), 20), BGRA(0, 0, 255)], [0, 0.2, 1]);
    gs := TBGRAGradientScanner.Create(g, gtLinear, p1, p2);
    ABitmap.FillRoundRectAntialias(0, 0, Width-BorderSpacing.InnerBorder -1,
      Height-BorderSpacing.InnerBorder -1, RoundX, RoundY, gs);
  end;

  ABitmap.RoundRectAntialias(BorderSpacing.InnerBorder div 2, BorderSpacing.InnerBorder div 2,
    Width-BorderSpacing.InnerBorder -1, Height-BorderSpacing.InnerBorder -1, RoundX, RoundY, CSSGray, BorderSpacing.InnerBorder);
end;

procedure TCustomLCControl.DrawControl;
var
  Bitmap: TBGRABitmap;
begin
  Bitmap := TBGRABitmap.Create(Width, Height, BGRAPixelTransparent);
  try
    DrawBackground(Bitmap, fStyle);
    DrawCaption(Bitmap, fStyle);
    //Bitmap.FillRoundRectAntialias(RoundX, RoundY, Height-RoundX, Width-RoundY,
    //  RoundX, RoundY, BGRA(0, 0, 255, 128));
    {draws a white square without transparency}
    //Bitmap.FillRect(20, 20, 60, 60, BGRAWhite, dmSet);
    {draws a transparent blue square}
    //bitmap.FillRect(40, 40, 80, 80, BGRA(0, 0, 255, 128), dmDrawWithTransparency);
    //bitmap.FillRect(0, 0, Height, Width, BGRA(0, 0, 255, 128), dmDrawWithTransparency);

    Bitmap.Draw(Canvas, 0, 0, False);
  finally
    Bitmap.Free;
  end;

  inherited DrawControl;
end;

end.

