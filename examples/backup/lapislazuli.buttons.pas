unit LapisLazuli.Buttons;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, BGRABitmap, BGRABitmapTypes, Math,
  BGRAGradients, BGRAGradientScanner, BGRAResample, BGRATextFX,
  LapisLazuli.BaseCtrls,
  LapisLazuli.BitmapUtils,
  LCLType, Graphics, Controls;

type
  TCustomLCButton = class(TCustomLCControl)
  private
    fStyleHover: TLCStyle;
    fStyleClick: TLCStyle;
    isMouseDown: boolean;
    procedure SetStyleHover(AValue: TLCStyle);
    procedure SetStyleClick(AValue: TLCStyle);
  protected
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DrawControl; override;
    property StyleHover: TLCStyle read fStyleHover write setStyleHover;
    property StyleClick: TLCStyle read fStyleHover write setStyleHover;
  end;


implementation

constructor TCustomLCButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fStyleHover := TLCStyle.Create(Self);
  fStyleClick := TLCStyle.Create(Self);
  isMouseDown := False;
end;

destructor TCustomLCButton.Destroy;
begin
  FreeAndNil(fStyleHover);
  FreeAndNil(fStyleClick);
  inherited Destroy;
end;

procedure TCustomLCButton.MouseEnter;
begin
  inherited MouseEnter;
  RenderControl;
  Invalidate;
end;

procedure TCustomLCButton.MouseLeave;
begin
  inherited MouseLeave;
  RenderControl;
  Invalidate;
end;

procedure TCustomLCButton.MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  isMouseDown := True;
  RenderControl;
  Invalidate;
end;

procedure TCustomLCButton.MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  isMouseDown := False;
  RenderControl;
  Invalidate;
end;

procedure TCustomLCButton.DrawControl;
var
  Bitmap: TBGRABitmap;
begin
  if MouseInClient then
  begin
    Bitmap := TBGRABitmap.Create(Width, Height, BGRAPixelTransparent);
    try
      DrawBackground(Bitmap, fStyleHover);
      DrawCaption(Bitmap, fStyleHover);
      Bitmap.Draw(Canvas, 0, 0, False);
    finally
      Bitmap.Free;
    end;
  end
  else if isMouseDown then
  begin
    Bitmap := TBGRABitmap.Create(Width, Height, BGRAPixelTransparent);
    try
      DrawBackground(Bitmap, fStyleClick);
      DrawCaption(Bitmap, fStyleClick);
      Bitmap.Draw(Canvas, 0, 0, False);
    finally
      Bitmap.Free;
    end;
  end
  else
    inherited DrawControl;
end;

procedure TCustomLCButton.SetStyleClick(AValue: TLCStyle);
begin
  if fStyleClick = AValue then
    Exit;
  fStyleClick.Assign(AValue);

  RenderControl;
  Invalidate;
end;

procedure TCustomLCButton.SetStyleHover(AValue: TLCStyle);
begin
  if fStyleHover = AValue then
    Exit;
  fStyleHover.Assign(AValue);

  RenderControl;
  Invalidate;
end;

end.

