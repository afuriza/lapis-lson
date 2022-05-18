unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Menus, ComCtrls, LapisLazuli.Buttons, LapisLazuli.Panels, LapisLazuli.ScrollBars, CSSCtrls,
  ATScrollBar, jsonparser;

type

  { TForm1 }

  TForm1 = class(TForm)
    ATScrollbar1: TATScrollbar;
    Label1: TLabel;
    LCButton1: TLCButton;
    LCPanel1: TLCPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    LCBtn: TCustomLCButton;
    LCSb: TCustomLCScrollbar;
  public
    procedure LCBtnClick(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.LCBtnClick(Sender: TObject);
begin
  //ShowMessage('Clicked '+LCBtn.Font.PixelsPerInch.ToString+' ppi, '+LCBtn.Font.Height.ToString);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  LCBtn := TCustomLCButton.Create(Self);
  LCSb := TCustomLCScrollbar.Create(Self);
  with LCSb do
  begin
    LCsb.Style.BorderWidth := 2;
    LCsb.Style.BorderColor := clBlack;
    Height := 10;
    Width := 50;
    Top := 0;
    Left := 0;
    Parent := Self;
  end;
  with LCBtn do
  begin
    Anchors := [akTop, akLeft, akBottom, akRight];
    OnClick := @LCBtnClick;
    Caption := 'Hello, 世界!';
  //  Style.GradientColors.Clear;
  //  Style.GradientColors.Add('clWhite');
  //  Style.GradientColors.Add('$00F5F5F5');
  //  Style.Font.Name := 'Ubuntu';
  //  Style.Rounding.RoundX := 5;
  //  Style.Rounding.RoundY := 5;
    //
    //StyleHover.GradientColors.Add('clWhite');
    //StyleHover.GradientColors.Add('$00E6E6E6');
    //StyleHover.Font.Name := 'Ubuntu';
    //StyleHover.Rounding.RoundX := 5;
    //StyleHover.Rounding.RoundY := 5;
    //
    //StyleClick.GradientColors.Add('$00E6E6E6');
    //StyleClick.GradientColors.Add('clWhite');
    //StyleClick.Font.Name := 'Ubuntu';
    //StyleClick.Rounding.RoundX := 5;
    //StyleClick.Rounding.RoundY := 5;
    //
    //BorderSpacing.InnerBorder := 1;
    Height := 100;
    Width := 150;
    Top := 20;
    Left := 40;
    Parent := Self;
    //UseStyle := True;
    //DoubleBuffered := True;
  end;
end;

procedure TForm1.FormPaint(Sender: TObject);
//var
//  x, y: Integer;
//  Bitmap: TBGRABitmap;
//  Rec: TRect;
begin
  //if Visible then
  //begin
  //  Brush.Style := bsClear;
  //  BorderStyle := bsNone;
  //end;
  //begin
  //Rec := TRect.Create(Left, Top, Top+Height, Left+Width);
  //Bitmap := TBGRABitmap.Create(100, 100, BGRAPixelTransparent);
  //try
  //  //Bitmap.FillRect(20, 20, 60, 60, BGRAWhite, dmSet); // draws a white square without transparency
  //  bitmap.FillRect(40, 40, 80, 80, BGRA(0, 0, 255, 128), dmDrawWithTransparency); // draws a transparent blue square
  //
  //  //Canvas.Draw(0, 0, Bitmap);
  //  Bitmap.Draw(Self.Canvas, 0, 0, False);
  //finally
  //  Bitmap.Free;
  //end;
  //
  //end;

end;

end.

