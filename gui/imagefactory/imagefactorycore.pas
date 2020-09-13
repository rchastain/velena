
unit imagefactorycore;

interface

uses
  Classes, SysUtils,
  BGRAGraphics, BGRABitmap, BGRABitmapTypes, BGRAGradientScanner;

procedure MakeDisk(const AScale: integer; const AColor: TBGRAPixel; const AGradient: boolean; const AFileName: string; const ALabel: boolean = FALSE);
procedure MakeGrid(const AScale: integer; const AColor: TBGRAPixel; const AGradient: boolean; const AFileName: string);
procedure MakeGridTexture(const AScale: integer; const ATextureName: string; const AFileName: string);
procedure MakeDiskTexture(const AScale: integer; const ATextureName: string; const AColor: TBGRAPixel; const AFileName: string);
procedure MakeBackground(const AScale: integer; const AAroundGridColor, ABehindGridColor: TBGRAPixel; const AFileName: string);

const
  CScales: array[0..2] of integer = (32, 40, 48);

procedure CreateImages(
  const AGridColor, AWhiteColor, ABlackColor, AAroundGridColor, ABehindGridColor: TBGRAPixel;
  const AStyle: string
);

procedure CreateImagesTexture(
  const ATextureName: string;
  const AWhiteColor, ABlackColor, AAroundGridColor, ABehindGridColor: TBGRAPixel;
  const AStyle: string
);

procedure MakePreview(const AScale: integer; const AStyle: string);

implementation

const
  CMargin = 4;

procedure MakeDisk(const AScale: integer; const AColor: TBGRAPixel; const AGradient: boolean; const AFileName: string; const ALabel: boolean = FALSE);
var
  LBitmap: TBGRABitmap;
  LScanner: TBGRACustomScanner;
begin
  LBitmap := TBGRABitmap.Create(AScale, AScale);

  if AGradient then
    LScanner := TBGRAGradientScanner.Create(
      ApplyIntensityFast(AColor, 50000),
      ApplyIntensityFast(AColor, 16000),
      gtRadial,
      PointF(AScale div 5, AScale div 5),
      PointF(AScale, AScale)
    )
  else
    LScanner := TBGRAConstantScanner.Create(AColor);

  LBitmap.FillEllipseAntialias(
    LBitmap.Width  / 2 - 0.5,
    LBitmap.Height / 2 - 0.5,
    LBitmap.Width  / 2 - CMargin div 2,
    LBitmap.Height / 2 - CMargin div 2,
    LScanner
  );

  LScanner.Free;

  if ALabel then
  begin
    LBitmap.FontName := 'Arial';
    LBitmap.FontAntialias := true;
    LBitmap.FontHeight := 3 * LBitmap.Height div 4;
    LBitmap.FontStyle := [fsBold];

    with LBitmap.FontPixelMetric do
      LBitmap.TextOut(
        LBitmap.Width div 2,
        LBitmap.Height div 2 - (CapLine + Baseline) div 2,
        '4',
        $000080,
        taCenter
      );
  end;

  LBitmap.SaveToFile(AFileName);
  LBitmap.Free;
end;

procedure MakeGrid(const AScale: integer; const AColor: TBGRAPixel; const AGradient: boolean; const AFileName: string);
var
  LResult, LMask: TBGRABitmap;
  LScanner: TBGRACustomScanner;
  x, y: integer;
begin
  LResult := TBGRABitmap.Create(
    AScale * 7 + 2 * CMargin,
    AScale * 6 + 2 * CMargin,
    BGRAPixelTransparent
  );

  if AGradient then
    LScanner := TBGRAGradientScanner.Create(
      ApplyIntensityFast(AColor, 50000),
      ApplyIntensityFast(AColor, 16000),
      gtRadial,
      PointF(AScale, AScale),
      PointF(8 * AScale, AScale)
    )
  else
    LScanner := TBGRAConstantScanner.Create(AColor);

  LResult.FillRoundRectAntialias(
    0, 0,
    AScale * 7 + 2 * CMargin - 1,
    AScale * 6 + 2 * CMargin - 1,
    8, 8,
    LScanner,
    []
  );
  LMask := TBGRABitmap.Create(
    AScale * 7 + 2 * CMargin,
    AScale * 6 + 2 * CMargin,
    BGRAWhite
  );
  for x := 1 to 7 do
    for y := 1 to 6 do
      LMask.FillEllipseAntialias(
        AScale * x - AScale div 2 - 0.5 + CMargin,
        AScale * y - AScale div 2 - 0.5 + CMargin,
        AScale div 2 - CMargin,
        AScale div 2 - CMargin,
        BGRABlack
      );
  LResult.ApplyMask(LMask);
  LResult.SaveToFile(AFileName);
  LResult.Free;
  LMask.Free;
end;

procedure MakeGridTexture(const AScale: integer; const ATextureName: string; const AFileName: string);
var
  LResult, LMask, LTexture: TBGRABitmap;
  x, y: integer;
begin
  LResult := TBGRABitmap.Create(
    AScale * 7 + 2 * CMargin,
    AScale * 6 + 2 * CMargin,
    BGRAPixelTransparent
  );
  LTexture := TBGRABitmap.Create(ATextureName);
  LResult.FillRoundRectAntialias(
    0, 0,
    AScale * 7 + 2 * CMargin - 1,
    AScale * 6 + 2 * CMargin - 1,
    8, 8,
    LTexture,
    []
  );
  LMask := TBGRABitmap.Create(
    AScale * 7 + 2 * CMargin,
    AScale * 6 + 2 * CMargin,
    BGRAWhite
  );
  for x := 1 to 7 do
    for y := 1 to 6 do
      LMask.FillEllipseAntialias(
        AScale * x - AScale div 2 - 0.5 + CMargin,
        AScale * y - AScale div 2 - 0.5 + CMargin,
        AScale div 2 - CMargin,
        AScale div 2 - CMargin,
        BGRABlack
      );
  LResult.ApplyMask(LMask);
  LResult.SaveToFile(AFileName);
  LResult.Free;
  LMask.Free;
  LTexture.Free;
end;

procedure MakeDiskTexture(const AScale: integer; const ATextureName: string; const AColor: TBGRAPixel; const AFileName: string);
var
  LBitmap: TBGRABitmap;
  LTexture: TBGRABitmap;
begin
  LTexture := TBGRABitmap.Create(ATextureName);
  LTexture.InplaceGrayscale;
  LTexture.FillRect(0, 0, LTexture.Width, LTexture.Height, AColor, dmDrawWithTransparency);
  
  LBitmap := TBGRABitmap.Create(AScale, AScale);
  LBitmap.FillEllipseAntialias(
    LBitmap.Width  / 2 - 0.5,
    LBitmap.Height / 2 - 0.5,
    LBitmap.Width  / 2 - CMargin div 2,
    LBitmap.Height / 2 - CMargin div 2,
    LTexture
  );

  LBitmap.SaveToFile(AFileName);
  LBitmap.Free;
  LTexture.Free;
end;

procedure MakeBackground(const AScale: integer; const AAroundGridColor, ABehindGridColor: TBGRAPixel; const AFileName: string);
var
  LBitmap: TBGRABitmap;
begin
  LBitmap := TBGRABitmap.Create(AScale * 9, AScale * 9, AAroundGridColor);
  LBitmap.FillRoundRectAntialias(
    AScale, 2 * AScale, 8 * AScale - 1, 8 * AScale - 1,
    8, 8,
    ABehindGridColor,
    []
  );
  LBitmap.SaveToFile(AFileName);
  LBitmap.Free;
end;

procedure CreateImages(
  const AGridColor, AWhiteColor, ABlackColor, AAroundGridColor, ABehindGridColor: TBGRAPixel;
  const AStyle: string
);
var
  i: integer;
begin
  for i := Low(CScales) to High(CScales) do
  begin
    ForceDirectories(Format('../../images/%s/%d', [AStyle, CScales[i]]));
    MakeDisk(CScales[i], AWhiteColor, TRUE, Format('../../images/%s/%d/white.png', [AStyle, CScales[i]]));
    MakeDisk(CScales[i], ABlackColor, TRUE, Format('../../images/%s/%d/black.png', [AStyle, CScales[i]]));
    MakeGrid(
      CScales[i],
      AGridColor,
      FALSE,
      Format('../../images/%s/%d/grid.png', [AStyle, CScales[i]])
    );
    MakeBackground(
      CScales[i],
      AAroundGridColor,
      ABehindGridColor,
      Format('../../images/%s/%d/background.png', [AStyle, CScales[i]])
    );
  end;
end;

procedure CreateImagesTexture(
  const ATextureName: string;
  const AWhiteColor, ABlackColor, AAroundGridColor, ABehindGridColor: TBGRAPixel;
  const AStyle: string
);
var
  i: integer;
begin
  for i := Low(CScales) to High(CScales) do
  begin
    ForceDirectories(Format('../../images/%s/%d', [AStyle, CScales[i]]));
    MakeGridTexture(CScales[i], ATextureName, Format('../../images/%s/%d/grid.png',  [AStyle, CScales[i]]));
    MakeDiskTexture(CScales[i], ATextureName, AWhiteColor, Format('../../images/%s/%d/white.png', [AStyle, CScales[i]]));
    MakeDiskTexture(CScales[i], ATextureName, ABlackColor, Format('../../images/%s/%d/black.png', [AStyle, CScales[i]]));
    MakeBackground(
      CScales[i],
      AAroundGridColor,
      ABehindGridColor,
      Format('../../images/%s/%d/background.png', [AStyle, CScales[i]])
    );
  end;
end;

procedure MakePreview(const AScale: integer; const AStyle: string);
var
  LResult, LBack, LWhiteDisk, LBlackDisk, LGrid: TBGRABitmap;
begin
  LResult := TBGRABitmap.Create(AScale * 9, AScale * 9);
  LBack := TBGRABitmap.Create(Format('../../images/%s/%d/background.png', [AStyle, AScale]));
  LWhiteDisk := TBGRABitmap.Create(Format('../../images/%s/%d/white.png', [AStyle, AScale]));
  LBlackDisk := TBGRABitmap.Create(Format('../../images/%s/%d/black.png', [AStyle, AScale]));
  LGrid := TBGRABitmap.Create(Format('../../images/%s/%d/grid.png', [AStyle, AScale]));

  LResult.PutImage(0, 0, LBack, dmSet);
  LResult.PutImage(3 * AScale, 7 * AScale, LWhiteDisk, dmDrawWithTransparency);
  LResult.PutImage(4 * AScale, 7 * AScale, LBlackDisk, dmDrawWithTransparency);
  LResult.PutImage(AScale - 4, AScale * 2 - 4, LGrid, dmDrawWithTransparency);

  LResult.SaveToFile(Format('../../images/%s/preview.png', [AStyle]));

  LResult.Free;
  LBack.Free;
  LWhiteDisk.Free;
  LBlackDisk.Free;
  LGrid.Free;
end;

end.
