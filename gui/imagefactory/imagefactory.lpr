
program imagefactory;

uses
  Classes, SysUtils,
  BGRAGraphics, BGRABitmap, BGRABitmapTypes, BGRAGradientScanner,
  imagefactorycore;

const
  CWood = 'resources/5-wood-textures/wood4.png';

begin
  CreateImages(
    BGRA($80, $80, $80, $D0),
    BGRA($FF, $D7, $00, $FF),
    BGRA($FF, $00, $00, $FF),
    CSSWhiteSmoke,
    CSSWhiteSmoke,
    'gray'
  );

  CreateImages(
    BGRA($00, $00, $FF, $D0),
    BGRA($FF, $D7, $00, $FF),
    BGRA($FF, $00, $00, $FF),
    CSSWhiteSmoke,
    CSSWhiteSmoke,
    'blue'
  );

  CreateImagesTexture(
    CWood,
    BGRA($8B, $00, $00, $C0),
    BGRA($20, $20, $20, $C0),
    CSSWhiteSmoke,
    CSSForestGreen,
    'wood'
  );

  MakePreview(CScales[High(CScales)], 'gray');
  MakePreview(CScales[High(CScales)], 'blue');
  MakePreview(CScales[High(CScales)], 'wood');

  (*
  // This requires to have a copy of arial.ttf file in the current directory.
  TBGRABitmap.AddFreeTypeFontFolder(GetCurrentDir);
  MakeDisk(256, $C0C0C0, TRUE, '../../images/icon.png', TRUE);
  *)
end.

