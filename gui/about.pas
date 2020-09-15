
unit About;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons;

(* https://github.com/wp-xyz/corona *)

type

  { TAboutForm }

  TAboutForm = class(TForm)
    BitBtn1: TBitBtn;
    IMIcon: TImage;
    LBCredits: TLabel;
    LBCompiler: TLabel;
    LBFPC: TLabel;
    LBAuthor: TLabel;
    LBGiuliano: TLabel;
    LBIDE: TLabel;
    LBLazarus: TLabel;
    LBTitle: TLabel;
    PNCredits: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LabelClick(Sender: TObject);
    procedure LabelMouseEnter(Sender: TObject);
    procedure LabelMouseLeave(Sender: TObject);
  private

  public

  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}

uses
  LCLIntf, Types, IntfGraphics, LazCanvas, FPCanvas;

const
  CUrlFPC = 'https://www.freepascal.org/';
  CUrlLazarus = 'https://www.lazarus-ide.org/';

procedure ScaleBitmap(BM: TBitmap; W, H: Integer);
var
  Source, Dest: TLazIntfImage;
  DestCanvas: TLazCanvas;
begin
  try
    Source := TLazIntfImage.Create(0, 0);
    Source.LoadFromBitmap(BM.Handle, 0);
    Dest := TLazIntfImage.Create(0, 0);
    Dest.LoadFromBitmap(BM.Handle, 0);
    DestCanvas := TLazCanvas.Create(Dest);
    DestCanvas.Interpolation := TFPBaseInterpolation.Create;
    DestCanvas.StretchDraw(0, 0, W, H, Source);
    BM.LoadFromIntfImage(Dest);
    BM.SetSize(W, H);
  finally
    DestCanvas.Interpolation.Free;
    DestCanvas.Free;
    Dest.Free;
    Source.Free;
  end;
end;

{ TAboutForm }

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  with IMIcon do
  begin
    Picture.Assign(Application.Icon);
    Picture.Icon.Current := Picture.Icon.GetBestIndexForSize(Size(128, 128));
  end;

  LBFPC.Hint := CUrlFPC;
  LBLazarus.Hint := CUrlLazarus;
end;

procedure TAboutForm.FormShow(Sender: TObject);
begin
  ScaleBitmap(IMIcon.Picture.Bitmap, IMIcon.Width, IMIcon.Height);
end;

procedure TAboutForm.LabelClick(Sender: TObject);
begin
  if Sender = LBFPC then
    OpenURL(CUrlFPC)
  else if Sender = LBLazarus then
    OpenURL(CUrlLazarus);
end;

procedure TAboutForm.LabelMouseEnter(Sender: TObject);
begin
  with (Sender as TControl).Font do
    Style := Style + [fsUnderline];
end;

procedure TAboutForm.LabelMouseLeave(Sender: TObject);
begin
  with (Sender as TControl).Font do
    Style := Style - [fsUnderline];
end;

end.

