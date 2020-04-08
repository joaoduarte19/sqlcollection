// ***************************************************************************
//
// SQLCollection
//
// Copyright (c) 2020 João Antônio Duarte
//
// https://github.com/joaoduarte19/sqlcollection
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ***************************************************************************

unit SQLCollection.Design.Utils;

interface

uses
  System.Classes,
  Winapi.Windows,
  Vcl.Graphics;

type
  TSQLCollectionDesignUtils = record
  private
    class procedure ConvertBitmapToGrayScale(ABitmap: TBitmap); static;
    class procedure FixGlyph(AGlyph: TBitmap); static;
  public

    class procedure FixComponentsGlyphs(AComponent: TComponent); static;
  end;

implementation

uses
  System.SysUtils,
  Vcl.ExtCtrls,
  Vcl.Buttons;

{ TSQLCollectionDesignUtils }

class procedure TSQLCollectionDesignUtils.ConvertBitmapToGrayScale(ABitmap: TBitmap);
var
  LRect: TRect;
  I, J: Integer;
  R, G, B: Byte;
  LRGBColor: Integer;
  LHCLR: Byte;
begin
  LRect := Rect(0, 0, ABitmap.Width, ABitmap.Height);

  for I := LRect.Left to LRect.Right do
  begin
    for J := LRect.Top to LRect.Bottom do
    begin

      LRGBColor := ColorToRGB(ABitmap.Canvas.Pixels[I, J]);

      R := GetRValue(LRGBColor);
      G := GetGValue(LRGBColor);
      B := GetBValue(LRGBColor);

      LHCLR := 0;
      if R > LHCLR then
        LHCLR := R;
      if G > LHCLR then
        LHCLR := G;
      if B > LHCLR then
        LHCLR := B;

      R := LHCLR;
      G := LHCLR;
      B := LHCLR;

      ABitmap.Canvas.Pixels[I, J] := PaletteRGB(R, G, B);
    end;
  end;
end;

class procedure TSQLCollectionDesignUtils.FixComponentsGlyphs(AComponent: TComponent);
var
  I: Integer;
  LSpeedButton: TSpeedButton;
  LBitBtn: TBitBtn;
begin
  for I := 0 to AComponent.ComponentCount - 1 do
  begin
    if AComponent.Components[I].ClassType = TPanel then
    begin
      TPanel(AComponent.Components[I]).ParentBackground := False;
    end
    else if AComponent.Components[I].ClassType = TSpeedButton then
    begin
      LSpeedButton := AComponent.Components[I] as TSpeedButton;
      if LSpeedButton.NumGlyphs <> 2 then
        FixGlyph(LSpeedButton.Glyph);
      LSpeedButton.NumGlyphs := 2;
    end
    else if AComponent.Components[I].ClassType = TBitBtn then
    begin
      LBitBtn := AComponent.Components[I] as TBitBtn;
      if LBitBtn.NumGlyphs <> 2 then
        FixGlyph(LBitBtn.Glyph);
      LBitBtn.NumGlyphs := 2;
    end;
  end;
end;

class procedure TSQLCollectionDesignUtils.FixGlyph(AGlyph: TBitmap);
var
  LWidth: Integer;
  LHeigth: Integer;
  LBmp1: TBitmap;
  LBmp2: TBitmap;
begin
  LBmp1 := TBitmap.Create;
  try
    LBmp2 := TBitmap.Create;
    try
      LWidth := AGlyph.Width;
      LHeigth := AGlyph.Height;

      LBmp1.Width := LWidth;
      LBmp1.Height := LHeigth;
      LBmp2.Width := LWidth;
      LBmp2.Height := LHeigth;

      LBmp1.Canvas.CopyRect(Rect(0, 0, LWidth, LHeigth), AGlyph.Canvas, Rect(0, 0, LWidth, LHeigth));
      LBmp2.Canvas.CopyRect(Rect(0, 0, LWidth, LHeigth), AGlyph.Canvas, Rect(0, 0, LWidth, LHeigth));

      ConvertBitmapToGrayScale(LBmp2);

      AGlyph.Assign(nil);
      AGlyph.Width := LBmp1.Width * 2;
      AGlyph.Height := LBmp1.Height;

      AGlyph.Canvas.CopyRect(Rect(0, 0, LBmp1.Width, LBmp1.Height), LBmp1.Canvas, Rect(0, 0, LBmp1.Width, LBmp1.Height));
      AGlyph.Canvas.CopyRect(Rect(LBmp2.Width, 0, LBmp2.Width * 2, LBmp2.Height), LBmp2.Canvas,
        Rect(0, 0, LBmp2.Width, LBmp2.Height));

      with AGlyph, Canvas do
        if Pixels[0, 0] <> Pixels[Width - 1, 0] then
        begin
          Pen.Color := Pixels[0, 0];
          Brush.Color := Pixels[0, 0];
          FloodFill(Width - 1, 0, Pixels[Width - 1, 0], fsSurface);
        end;

    finally
      FreeAndNil(LBmp2);
    end;
  finally
    FreeAndNil(LBmp1);
  end;
end;

end.
