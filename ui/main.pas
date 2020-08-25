
unit main;
{$MODE OBJFPC}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, LCLType, Menus, LCLIntf, Process, RegExpr,
{$IFDEF UNIX}
  BaseUnix,
{$ENDIF UNIX}
  BGRABitmap, BGRABitmapTypes, BGRAGradientScanner,
  LightweightTranslationManager,
  Grid;

type
  TPlayers = array[boolean] of boolean;

const
  CGameModes: array[0..2] of TPlayers = (
    (FALSE, TRUE),
    (TRUE, FALSE),
    (FALSE, FALSE)
  );

  CColorGrid  = 1;
  CColorEmpty = 2;
  CColorWhite = 3;
  CColorBlack = 4;

type
  TAnimation = record
    Done: boolean;
    x, y: integer;
  end;


  { TForm1 }

  TForm1 = class(TForm)
    BTInsert: TButton;
    LBMessage: TLabel;
    MMMenu: TMainMenu;
    MMEngineOutput: TMemo;
    MIMode: TMenuItem;
    MIComputerWhite: TMenuItem;
    MIComputerBlack: TMenuItem;
    MIHumanVsHuman: TMenuItem;
    MIWeak: TMenuItem;
    MINormal: TMenuItem;
    MIStrong: TMenuItem;
    MIRestart: TMenuItem;
    MIQuit: TMenuItem;
    MIFile: TMenuItem;
    MIOptions: TMenuItem;
    MIHelp: TMenuItem;
    MILevel: TMenuItem;
    MIAbout: TMenuItem;
    TMTimer: TTimer;
    procedure BTInsertClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure MIComputerBlackClick(Sender: TObject);
    procedure MIComputerWhiteClick(Sender: TObject);
    procedure MIHumanVsHumanClick(Sender: TObject);
    procedure MINormalClick(Sender: TObject);
    procedure MIRestartClick(Sender: TObject);
    procedure MIStrongClick(Sender: TObject);
    procedure MIWeakClick(Sender: TObject);
    procedure MMEngineOutputChange(Sender: TObject);
    procedure MIAboutClick(Sender: TObject);
    procedure MIQuitClick(Sender: TObject);
    procedure TMTimerTimer(Sender: TObject);
  private
    FAnimation: TAnimation;
    FButtons: array[1..7] of TButton;
    FVisualGrid: array[1..7, 1..7] of integer;
    FGrid: TGrid;
    FBitmap: TBGRABitmap;
    FDisk: TDisk;
    FDiskBitmaps: array[CColorEmpty..CColorBlack] of TBGRABitmap;
    FProcess: TProcess;
    FWaiting: boolean;
    FColors: array[CColorGrid..CColorBlack] of TBGRAPixel;
    FPlayers: TPlayers;
    FGameOver: boolean;
    FHistory: string;
    FExpr: TRegExpr;
    FLevel: char;
    procedure InsertDisk(const x: integer);
    procedure CreateDiskBitmaps;
    procedure DrawDisk(const x, y: integer; const AColor: integer);
    procedure PaintImage;
    procedure Restart;
    procedure AskEngineMove(const AHistory: string);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure LogLn(const ALine: string; const ARewrite: boolean = FALSE);
var
  LFileName: string;
  LFile: text;
begin
  LFileName := ChangeFileExt(ParamStr(0), '.log');
  Assign(LFile, LFileName);
  if FileExists(LFileName) and not ARewrite then
    Append(LFile)
  else
    Rewrite(LFile);
  WriteLn(LFile, ALine);
  Close(LFile);
end;

procedure SetPixel(var APixel: TBGRAPixel; const AColor: longword);
begin
  APixel.Blue  := (AColor and $FF0000) shr 16;
  APixel.Green := (AColor and $00FF00) shr 8;
  APixel.Red   := (AColor and $0000FF);
  APixel.Alpha := $FF;
end;

procedure TForm1.FormCreate(Sender: TObject);
const
  CExecutable = 'connect4'{$IFDEF WINDOWS} + '.exe'{$ENDIF WINDOWS};

var
  x: integer;
begin
  LogLn(DateTimeToStr(Now), TRUE);
  for x := 1 to 7 do
  begin
    FButtons[x] := TButton.Create(Self);
    with FButtons[x] do
    begin
      Parent := Self;
      Name   := Chr(x - 1 + Ord('A'));
      Left   := 48 * (x - 1) + 24 + 4;
      Top    := 384 + 12;
      Width  := 40;
      Height := 24;
      Tag    := x;
      OnClick := @BTInsertClick;
    end;
  end;

  SetPixel(FColors[CColorGrid],  $707070);
  SetPixel(FColors[CColorEmpty], $808080);
  SetPixel(FColors[CColorWhite], $C0C0C0);
  SetPixel(FColors[CColorBlack], $202020);
  FBitmap := TBGRABitmap.Create(8 * 48, 8 * 48, FColors[1]);
  CreateDiskBitmaps;

  FProcess := TProcess.Create(Self);
  FProcess.Options := [poUsePipes, poStderrToOutPut, poNoConsole];

  if FileExists(CExecutable) then
  begin
{$IFDEF UNIX}
    FpChmod(CExecutable, &777);
{$ENDIF UNIX}
    FProcess.Executable := CExecutable;
    FProcess.Execute;
    MIComputerBlack.Click;
  end else
  begin
    ShowMessage('Cannot find ' + CExecutable);
    MIHumanVsHuman.Click;
  end;

  FExpr := TRegExpr.Create('Velena answers in (\d)');

  (*
  Height := 478;
  *)
  (*
  Height := LBMessage.Top + LBMessage.Height + LCLIntf.GetSystemMetrics(SM_CYCAPTION) + 2 * LCLIntf.GetSystemMetrics(SM_CYSIZEFRAME);
  *)

  TransMgr.IniFile := ExtractFilePath(Application.ExeName) + 'lang.cfg';
  if Application.HasOption('l', 'lang') then
    TransMgr.Language := Application.GetOptionValue('lang')
  else
    TransMgr.Language := 'en-gb';
  LogLn(Format('TransMgr.LanguageName=%s', [TransMgr.LanguageName]));
  Caption := TransMgr.GetString('appname');
  LBMessage.Caption := TransMgr.GetString('whitetomove');

  FWaiting := FALSE;
  FAnimation.Done := TRUE;
  MIStrong.Click;
  Restart;
end;

procedure TForm1.BTInsertClick(Sender: TObject);
var
  x: integer;
begin
  if not FAnimation.Done then
    Exit;

  if FGameOver then
  begin
    Restart;
    Exit;
  end;

  x := (Sender as TButton).Tag;
  if FGrid[x, 6] = CEmpty then
    InsertDisk(x);
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  LCommand: string;
  i: integer;
begin
  FBitmap.Free;
  for i := CColorEmpty to CColorBlack do
    FDiskBitmaps[i].Free;
  if FProcess.Running then
  begin
    (* Ask Velena to stop. *)
    LCommand := 'quit' + LineEnding;
    FProcess.Input.Write(LCommand[1], Length(LCommand));
    LogLn('> ' + LCommand);
    FProcess.Terminate(0);
  end;
  FProcess.Free;
  FExpr.Free;
end;

procedure TForm1.FormPaint(Sender: TObject);
var
  x, y: integer;
begin
  (*
  FBitmap.FillRect(0, 0, FBitmap.Width, FBitmap.Height, FColors[1], dmSet);
  *)
  for x := 1 to 7 do
  begin
    if (not FAnimation.Done) and (x <> FAnimation.x) then
      Continue;
    FBitmap.FillRect(48 * x - 24, 0, 48 * x + 24, FBitmap.Height, FColors[CColorGrid], dmSet);
    for y := 1 to 7 do
      DrawDisk(48 * x - 24, 7 * 48 - (48 * y - 24), FVisualGrid[x, y]);
  end;
  PaintImage;
end;

procedure TForm1.MIComputerBlackClick(Sender: TObject);
begin
  FPlayers := CGameModes[0];
  MIComputerWhite.Checked := FALSE;
  MIComputerBlack.Checked := TRUE;
  MIHumanVsHuman.Checked := FALSE;
  MILevel.Enabled := TRUE;
end;

procedure TForm1.MIComputerWhiteClick(Sender: TObject);
begin
  FPlayers := CGameModes[1];
  MIComputerWhite.Checked := TRUE;
  MIComputerBlack.Checked := FALSE;
  MIHumanVsHuman.Checked := FALSE;
  MILevel.Enabled := TRUE;
end;

procedure TForm1.MIHumanVsHumanClick(Sender: TObject);
begin
  FPlayers := CGameModes[2];
  MIComputerWhite.Checked := FALSE;
  MIComputerBlack.Checked := FALSE;
  MIHumanVsHuman.Checked := TRUE;
  MILevel.Enabled := FALSE;
end;

procedure TForm1.MINormalClick(Sender: TObject);
begin
  FLevel := 'b';
  MIWeak.Checked := FALSE;
  MINormal.Checked := TRUE;
  MIStrong.Checked := FALSE;
end;

procedure TForm1.MIRestartClick(Sender: TObject);
begin
  Restart;
  Invalidate;
end;

procedure TForm1.MIStrongClick(Sender: TObject);
begin
  FLevel := 'c';
  MIWeak.Checked := FALSE;
  MINormal.Checked := FALSE;
  MIStrong.Checked := TRUE;
end;

procedure TForm1.MIWeakClick(Sender: TObject);
begin
  FLevel := 'a';
  MIWeak.Checked := TRUE;
  MINormal.Checked := FALSE;
  MIStrong.Checked := FALSE;
end;

procedure TForm1.MMEngineOutputChange(Sender: TObject);
begin
  LogLn(Format('Memo.Text = {%s%s}', [LineEnding, MMEngineOutput.Text]));
  if Length(MMEngineOutput.Text) = 0 then
    Exit;
  if FExpr.Exec(MMEngineOutput.Text) then
  begin
    FWaiting := FALSE;
    InsertDisk(StrToInt(FExpr.Match[1]));
  end;
end;

procedure TForm1.MIAboutClick(Sender: TObject);
begin
  ShowMessage('Connect 4 engine by Giuliano Bertoletti, with a new GUI.');
end;

procedure TForm1.MIQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.TMTimerTimer(Sender: TObject);
var
  LColor: integer;
  LMessage: string;
  LNoMoreOutput: boolean;

  procedure ReadProcessOutput(AProcess: TProcess; AMemo: TMemo);
  var
    LBuffer: string = '';
    LBytesAvailable: DWord;
    LBytesRead: LongInt;
  begin
    if AProcess.Running then
    begin
      LBytesAvailable := AProcess.Output.NumBytesAvailable;
      LBytesRead := 0;
      while LBytesAvailable > 0 do
      begin
        SetLength(LBuffer, LBytesAvailable);
        LBytesRead := AProcess.OutPut.Read(LBuffer[1], LBytesAvailable);
        AMemo.Text := AMemo.Text + Copy(LBuffer, 1, LBytesRead);
        LBytesAvailable := AProcess.Output.NumBytesAvailable;
        LNoMoreOutput := FALSE;
      end;
      if LBytesRead > 0 then
        AMemo.SelStart := Length(AMemo.Text);
    end else
    begin
      TMTimer.Enabled := FALSE;
      ShowMessage('Velena stopped unexpectedly.');
    end;
  end;
begin
  if FGameOver then
    Exit;
  if not FAnimation.Done then
  begin
    if (FAnimation.y = 1) or (FVisualGrid[FAnimation.x, FAnimation.y - 1] <> CColorEmpty) then
    begin
      FAnimation.Done := TRUE;
      LMessage := '';
      if GridValue(CBlack, FGrid) >= 4 then
        LMessage := TransMgr.GetString('blackwins')
      else if GridValue(CWhite, FGrid) >= 4 then
        LMessage := TransMgr.GetString('whitewins')
      else if IsFull(FGrid) then
        LMessage := TransMgr.GetString('draw');
      if Length(LMessage) > 0 then
      begin
        FGameOver := TRUE;
        LBMessage.Caption := LMessage;
      end else
        if FDisk = CWhite then
          LBMessage.Caption := TransMgr.GetString('whitetomove')
        else
          LBMessage.Caption := TransMgr.GetString('blacktomove');
    end else
    begin
      LColor := FVisualGrid[FAnimation.x, FAnimation.y];
      if FAnimation.y < 7 then
        FVisualGrid[FAnimation.x, FAnimation.y] := CColorEmpty
      else
        FVisualGrid[FAnimation.x, FAnimation.y] := CColorGrid;
      Dec(FAnimation.y);
      FVisualGrid[FAnimation.x, FAnimation.y] := LColor;
      Invalidate;
    end;
  end else
    if not FWaiting then
      if (FDisk = CWhite) and FPlayers[FALSE]
      or (FDisk = CBlack ) and FPlayers[TRUE] then
        AskEngineMove(FHistory);

  repeat
    LNoMoreOutput := TRUE;
    ReadProcessOutput(FProcess, MMEngineOutput);
  until LNoMoreOutput;
end;

procedure TForm1.CreateDiskBitmaps;
var
  i: integer;
  LScanner: TBGRACustomScanner;
begin
  for i := CColorEmpty to CColorBlack do
  begin
    FDiskBitmaps[i] := TBGRABitmap.Create(48, 48, FColors[CColorGrid]);
    (*
    FDiskBitmaps[i].FillRect(0, 0, FDiskBitmaps[i].Width, FDiskBitmaps[i].Height, FColors[1], dmSet);
    *)
    if i = 2 then
      LScanner := TBGRAConstantScanner.Create(FColors[i])
    else
      LScanner := TBGRAGradientScanner.Create(ApplyIntensityFast(FColors[i], 50000), ApplyIntensityFast(FColors[i], 16000), gtRadial, PointF(8, 8), PointF(48, 48));
    FDiskBitmaps[i].FillEllipseAntialias(FDiskBitmaps[i].Width / 2, FDiskBitmaps[i].Height / 2, 7 * FDiskBitmaps[i].Width / 16, 7 * FDiskBitmaps[i].Height / 16, LScanner);
    LScanner.Free;
  end;
end;

procedure TForm1.DrawDisk(const x, y: integer; const AColor: integer);
begin
  if AColor = CColorGrid then
    Exit;
  FBitmap.PutImage(x, y, FDiskBitmaps[AColor], dmSet);
end;

procedure TForm1.PaintImage;
begin
  FBitmap.Draw(Canvas, 0, 0, TRUE);
end;

procedure TForm1.Restart;
var
  x, y: integer;
begin
  EmptyGrid(FGrid);
  FDisk := CWhite;
  FGameOver := FALSE;
  FHistory := '';
  for x := 1 to 7 do
    for y := 1 to 6 do
      FVisualGrid[x, y] := CColorEmpty;
  for x := 1 to 7 do
    FVisualGrid[x, 7] := CColorGrid;
end;

procedure TForm1.InsertDisk(const x: integer);
begin
  if FDisk = CWhite then
    FVisualGrid[x, 7] := CColorWhite
  else
    FVisualGrid[x, 7] := CColorBlack;

  Invalidate;
  FAnimation.Done := FALSE;
  FAnimation.x := x;
  FAnimation.y := 7;

  if Grid.InsertDisk(FGrid, FDisk, x) then
  begin
    FDisk := CBlack * FDisk;
    FHistory := FHistory + IntToStr(x);
  end else
    LogLn('Something is rotten in this program.');
end;

procedure TForm1.AskEngineMove(const AHistory: string);
var
  LCommand: string;
begin
  if FProcess.Running then
  begin
    MMEngineOutput.Clear;
    (* Ask Velena to play. *)
    LCommand := Format('%s%s0' + LineEnding, [FLevel, AHistory]);
    FProcess.Input.Write(LCommand[1], Length(LCommand));
    LogLn('> ' + LCommand);
    FWaiting := TRUE;
  end;
end;

end.
