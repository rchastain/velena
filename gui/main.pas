
unit Main;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF UNIX}
  BaseUnix,
{$ENDIF UNIX}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
  StdCtrls, LCLIntf, Process, RegExpr,
  (* BGRABitmap *)
  BGRABitmap, BGRABitmapTypes,
  (* https://www.lazarusforum.de/viewtopic.php?t=11928 *)
  Translation,
  (* Connect Four grid *)
  Grid;

{$I version.inc}

type
  TPlayers = array[boolean] of boolean;

const
  CGameModes: array[0..2] of TPlayers = (
    (FALSE, TRUE),
    (TRUE, FALSE),
    (FALSE, FALSE)
  );

type
  { TForm1 }
  TForm1 = class(TForm)
    MIComputerWhite: TMenuItem;
    MIComputerBlack: TMenuItem;
    MIHumanVsHuman: TMenuItem;
    MIWeak: TMenuItem;
    MINormal: TMenuItem;
    MIStrong: TMenuItem;
    MILevel: TMenuItem;
    MIMode: TMenuItem;
    MIHelp: TMenuItem;
    MIAbout: TMenuItem;
    MINew: TMenuItem;
    MISeparator1: TMenuItem;
    MISeparator2: TMenuItem;
    MIGravity: TMenuItem;
    MISmall: TMenuItem;
    MIMedium: TMenuItem;
    MILarge: TMenuItem;
    MMMenu: TMainMenu;
    MIGame: TMenuItem;
    MIQuit: TMenuItem;
    MIOptions: TMenuItem;
    MISize: TMenuItem;
    PBGrid: TPaintBox;
    STMessage: TStaticText;
    TMTimer: TTimer;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure MIAboutClick(Sender: TObject);
    procedure MIComputerBlackClick(Sender: TObject);
    procedure MIComputerWhiteClick(Sender: TObject);
    procedure MIHumanVsHumanClick(Sender: TObject);
    procedure MINewClick(Sender: TObject);
    procedure MINormalClick(Sender: TObject);
    procedure MISmallClick(Sender: TObject);
    procedure MIMediumClick(Sender: TObject);
    procedure MILargeClick(Sender: TObject);
    procedure MIQuitClick(Sender: TObject);
    procedure MIStrongClick(Sender: TObject);
    procedure MIWeakClick(Sender: TObject);
    procedure PBGridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PBGridMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PBGridPaint(Sender: TObject);
    procedure TMTimerTimer(Sender: TObject);
  private
    FBuffer, FBackground, FWhiteDisk, FBlackDisk, FGridBitmap: TBGRABitmap;
    FAnimation: record
      Done: boolean;
      x, y, dy: integer;
    end;
    FStyle: string;
    FScale: integer;
    FSideToMove: boolean;
    FGrid: TGrid;
    FRow: integer;
    FGameOver: boolean;
    FPlayers: TPlayers;
    FProcess: TProcess;
    FEngineOutput: TStringList;
    FWaiting: boolean;
    FHistory: string;
    FExpr: TRegExpr;
    FLevel: char;
    procedure StyleMenuItemClick(Sender: TObject);
    procedure CreateBitmaps(const AStyle: string; const AScale: integer);
    procedure ResizeComponents(const AScale: integer);
    procedure SetScale(const AScale: integer);
    procedure SetStyle(const AStyle: string);
    procedure StartAnimation(const AColumn: integer);
    procedure Animation(const AGravity: boolean);
    procedure StopAnimation;
    procedure DrawStaticDisks;
    procedure Draw(const ACreate, AInvalidate: boolean);
    procedure AskEngineToPlay(const ALevel: char; const AHistory: string);
  public

  end;

procedure CreateStyleMenu(AParent: TMenuItem; AClickEvent: TNotifyEvent);

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

function GetImagePath: string;
begin
  result := ExtractFilePath(Application.ExeName) + 'images' + PathDelim;
end;

const
  CDefaultStyle = 'blue';
  CDefaultScale = 48;
  CDefaultLanguage = 'en-gb';
  CExecutable = 'connect4'{$IFDEF WINDOWS} + '.exe'{$ENDIF WINDOWS};

(* Create a menu item for each folder found in 'images' folder. *)
procedure CreateStyleMenu(AParent: TMenuItem; AClickEvent: TNotifyEvent);
function Capitalize(const AName: string): string;
begin
  if Length(AName) > 0 then
    result := UpCase(AName[1]) + Copy(AName, 2, Length(AName) - 1)
  else
    result := '';
end;
var
  LItems: array of TMenuItem;
  LRec: TSearchRec;
  LList: TStringList;
  i: integer;
begin
  if AParent = nil then
    Exit;
  Initialize(LItems);
  SetLength(LItems, 0);
  LList := TStringList.Create;
  LList.Sorted := TRUE;
  if FindFirst(GetImagePath + '*', faAnyFile or faDirectory, LRec) = 0 then
  repeat
    with LRec do
      if (Attr and faDirectory) = faDirectory then
      begin
        if (Name = '.')
        or (Name = '..') then
          Continue;
        LList.Append(Name);
      end;
  until FindNext(LRec) <> 0;
  FindClose(LRec);
  SetLength(LItems, LList.Count);
  for i := 0 to Pred(LList.Count) do
  begin
    LItems[i] := TMenuItem.Create(AParent);
    LItems[i].Caption := Capitalize(LList[i]);
    LItems[i].Hint := LList[i]; (* Folder name *)
    LItems[i].Tag := 0;
    LItems[i].OnClick := AClickEvent;
    LItems[i].RadioItem := TRUE;
    LItems[i].AutoCheck := TRUE;
    LItems[i].Checked := LList[i] = CDefaultStyle;
  end;
  LList.Free;
  AParent.Add(NewSubMenu('Style', 0, 'MIStyle', LItems));
  SetLength(LItems, 0);
end;

{ TForm1 }

procedure TForm1.StyleMenuItemClick(Sender: TObject);
var
  LMenuItem: TMenuItem absolute Sender;
begin
  if Sender is TMenuItem then
    SetStyle(LMenuItem.Hint);
end;

procedure TForm1.CreateBitmaps(const AStyle: string; const AScale: integer);
begin
  if Assigned(FBuffer) then FBuffer.Free;
  FBuffer := TBGRABitmap.Create(AScale * 9, AScale * 9);
  BGRAReplace(FBackground, TBGRABitmap.Create(GetImagePath + Format('%s/%d/background.png', [AStyle, AScale])));
  BGRAReplace(FWhiteDisk, TBGRABitmap.Create(GetImagePath + Format('%s/%d/white.png', [AStyle, AScale])));
  BGRAReplace(FBlackDisk, TBGRABitmap.Create(GetImagePath + Format('%s/%d/black.png', [AStyle, AScale])));
  BGRAReplace(FGridBitmap, TBGRABitmap.Create(GetImagePath + Format('%s/%d/grid.png', [AStyle, AScale])));
end;

procedure TForm1.ResizeComponents(const AScale: integer);
begin
  PBGrid.Width := 9 * AScale;
  PBGrid.Height := 9 * AScale;
  STMessage.Width := 9 * AScale;
  STMessage.Top := 9 * AScale;
end;

procedure TForm1.Draw(const ACreate, AInvalidate: boolean);
var
  LDisk: TBGRABitmap;
begin
  if ACreate then
    CreateBitmaps(FStyle, FScale);

  FBuffer.PutImage(0, 0, FBackground, dmSet);

  if ACreate then
    DrawStaticDisks
  else (* Animation procedure *)
  begin
    if FSideToMove then LDisk := FBlackDisk else LDisk := FWhiteDisk;
    FBuffer.PutImage(FAnimation.x, FAnimation.y, LDisk, dmDrawWithTransparency);
  end;

  FBuffer.PutImage(FScale - 4, FScale * 2 - 4, FGridBitmap, dmDrawWithTransparency);

  if AInvalidate then
    PBGrid.Invalidate;
end;

procedure TForm1.SetScale(const AScale: integer);
begin
  FScale := AScale;
  ResizeComponents(FScale);
  Draw(TRUE, TRUE);
end;

procedure TForm1.SetStyle(const AStyle: string);
begin
  FStyle := AStyle;
  Draw(TRUE, TRUE);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;
  LogLn('** ' + CAppInfo, TRUE);
  LogLn('** ' + CAppName + ' started at ' + TimeToStr(Time) + '.');
  CreateStyleMenu(MIOptions, @StyleMenuItemClick);

  FStyle := CDefaultStyle;
  FScale := CDefaultScale;

  EmptyGrid(FGrid);
  ResizeComponents(FScale);
  Draw(TRUE, FALSE);

  FAnimation.Done := TRUE;
  FSideToMove := FALSE; (* False = White *)
  FGameOver := FALSE;
  FHistory := '';

  MIComputerBlack.Click;
  MIStrong.Click;

  FEngineOutput := TStringList.Create;
  FProcess := TProcess.Create(Self);
  FProcess.Options := [poUsePipes, poStderrToOutPut, poNoConsole];
  FProcess.Executable := CExecutable;

  FExpr := TRegExpr.Create('Velena answers in (\d)');

  TransMgr.IniFile := ExtractFilePath(Application.ExeName) + 'lang.cfg';
  if Application.HasOption('l', 'lang') then
    TransMgr.Language := Application.GetOptionValue('l', 'lang')
  else
    TransMgr.Language := CDefaultLanguage;
  LogLn('** Language: ' + TransMgr.LanguageName);
  Caption := TransMgr.GetString('appname');
  STMessage.Caption := TransMgr.GetString('whitetomove');
  MIGame.Caption := TransMgr.GetString('game');
  MINew.Caption := TransMgr.GetString('newgame');
  MIQuit.Caption := TransMgr.GetString('exit');
  MIOptions.Caption := TransMgr.GetString('options');
  MIComputerWhite.Caption := TransMgr.GetString('computerwhite');
  MIComputerBlack.Caption := TransMgr.GetString('computerblack');
  MIHumanVsHuman.Caption := TransMgr.GetString('humanvshuman');
  MIHelp.Caption := TransMgr.GetString('help');
  MIAbout.Caption := TransMgr.GetString('about');

  FWaiting := FALSE;
  TMTimer.Enabled := TRUE;

  if FileExists(CExecutable) then
  begin
{$IFDEF UNIX}
    FpChmod(CExecutable, &777);
{$ENDIF UNIX}
    FProcess.Execute;
  end else
  begin
    LogLn('** Cannot find ''' + CExecutable + '''.');
    MIHumanVsHuman.Click;
    MIComputerWhite.Enabled := FALSE;
    MIComputerBlack.Enabled := FALSE;
  end;
end;

procedure TForm1.MIAboutClick(Sender: TObject);
begin
  ShowMessage('Velena Connect Four Engine by Giuliano Bertoletti.');
end;

procedure TForm1.MIComputerBlackClick(Sender: TObject);
begin
  FPlayers := CGameModes[0];
  MILevel.Enabled := TRUE;
end;

procedure TForm1.MIComputerWhiteClick(Sender: TObject);
begin
  FPlayers := CGameModes[1];
  MILevel.Enabled := TRUE;
end;

procedure TForm1.MIHumanVsHumanClick(Sender: TObject);
begin
  FPlayers := CGameModes[2];
  MILevel.Enabled := FALSE;
end;

procedure TForm1.MINewClick(Sender: TObject);
begin
  EmptyGrid(FGrid);
  Draw(TRUE, TRUE);
  FAnimation.Done := TRUE;
  FSideToMove := FALSE; (* False = White *)
  FGameOver := FALSE;
  FHistory := '';
  STMessage.Caption := TransMgr.GetString('whitetomove');
end;

procedure TForm1.MINormalClick(Sender: TObject);
begin
  FLevel := 'b';
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  LCommand: string;
begin
  FBuffer.Free;
  FBackground.Free;
  FWhiteDisk.Free;
  FBlackDisk.Free;
  FGridBitmap.Free;
  if FProcess.Running then
  begin
    (* Ask Velena to stop. *)
    LCommand := 'quit' + LineEnding;
    FProcess.Input.Write(LCommand[1], Length(LCommand));
    LogLn('>> ' + Trim(LCommand));
    FProcess.Terminate(0);
  end;
  FProcess.Free;
  FEngineOutput.Free;
  FExpr.Free;
end;

procedure TForm1.StartAnimation(const AColumn: integer);
begin
  FAnimation.Done := FALSE;
  FAnimation.x := FScale * AColumn;
  FAnimation.y := FScale;
  FAnimation.dy := 0;
  FBuffer.ClipRect := Rect(FAnimation.x, FScale, FAnimation.x + FScale, FScale * (9 - FRow));
end;

procedure TForm1.DrawStaticDisks;
var
  x, y: integer;
  LDisk: TBGRABitmap;
begin
  for y := 6 downto 1 do
    for x := 1 to 7 do
    begin
      case FGrid[x, y] of
        CWhite: LDisk := FWhiteDisk;
        CBlack: LDisk := FBlackDisk;
        CEmpty: LDisk := nil;
      end;
      if Assigned(LDisk) then
        FBuffer.PutImage(FScale * x, FScale * (8 - y), LDisk, dmDrawWithTransparency);
    end;
end;

procedure TForm1.MISmallClick(Sender: TObject);
begin
  SetScale(32);
end;

procedure TForm1.MIMediumClick(Sender: TObject);
begin
  SetScale(40);
end;

procedure TForm1.MILargeClick(Sender: TObject);
begin
  SetScale(48);
end;

procedure TForm1.MIQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.MIStrongClick(Sender: TObject);
begin
  FLevel := 'c';
end;

procedure TForm1.MIWeakClick(Sender: TObject);
begin
  FLevel := 'a';
end;

procedure TForm1.PBGridMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if (X < FScale)
  or (X > FScale * 8) then
    PBGrid.Cursor := crDefault
  else
    PBGrid.Cursor := crHandPoint;
end;

procedure TForm1.PBGridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LCol: integer;
begin
  if (X < FScale)
  or (X > FScale * 8)
  or (not FAnimation.Done) then
    Exit;
  LCol := X div FScale;
  if FGrid[LCol, 6] <> CEmpty then
    Exit;
  FRow := InsertDisk(FGrid, FSideToMove, LCol);
  FHistory := FHistory + IntToStr(LCol);
  StartAnimation(LCol);
end;

procedure TForm1.PBGridPaint(Sender: TObject);
begin
  FBuffer.Draw(PBGrid.Canvas, 0, 0, TRUE);
end;

procedure TForm1.StopAnimation;
var
  s: string;
begin
  FAnimation.Done := TRUE;
  FSideToMove := not FSideToMove;
  LogLn(GridToText(FGrid));
  s := '';
  if GridValue(CBlack, FGrid) >= 4 then
    s := TransMgr.GetString('blackwins')
  else if GridValue(CWhite, FGrid) >= 4 then
    s := TransMgr.GetString('whitewins')
  else if IsFull(FGrid) then
    s := TransMgr.GetString('draw');
  if Length(s) > 0 then
    FGameOver := TRUE
  else
    if FSideToMove then
      s := TransMgr.GetString('blacktomove')
    else
      s := TransMgr.GetString('whitetomove');
  STMessage.Caption := s;
end;

procedure TForm1.Animation(const AGravity: boolean);
var
  LNextTurn: boolean;
  LCushion: integer;
  YMax: integer;
begin
  if AGravity then
  begin
    Inc(FAnimation.y, FAnimation.dy);
    Inc(FAnimation.dy, FScale div 12);  (* Gravity *)
    YMax := FScale * (8 - FRow);
    LNextTurn := FALSE;
    if FAnimation.y >= YMax then        (* Touch bottom *)
    begin
      FAnimation.y := YMax;
      LCushion := FScale div 4;
      if FAnimation.dy >= LCushion then (* Enough energy to bounce? *)
        FAnimation.dy := -1 * (FAnimation.dy - LCushion) div 2
      else
      begin
        FAnimation.dy := 0;
        LNextTurn := TRUE;
      end;
    end;
    Draw(FALSE, TRUE);
    if LNextTurn then
      StopAnimation;
  end else
  (* Simple animation *)
  begin
    Draw(FALSE, TRUE);
    if FAnimation.y < FScale * (8 - FRow) then
      Inc(FAnimation.y, FScale div 4)
    else
      StopAnimation;
  end;
end;

procedure TForm1.TMTimerTimer(Sender: TObject);
var
  LDone: boolean;
  procedure ReadOutput(AProcess: TProcess);
  var
    LBuffer: string = '';
    LBytesAvailable: dword;
    LBytesRead: longint;
  begin
    if not AProcess.Running then
      Exit;
    LBytesAvailable := AProcess.Output.NumBytesAvailable;
    LBytesRead := 0;
    while LBytesAvailable > 0 do
    begin
      SetLength(LBuffer, LBytesAvailable);
      LBytesRead := AProcess.OutPut.Read(LBuffer[1], LBytesAvailable);
      FEngineOutput.Text := FEngineOutput.Text + Copy(LBuffer, 1, LBytesRead);
      LBytesAvailable := AProcess.Output.NumBytesAvailable;
      LDone := FALSE;
    end;
  end;
var
  LCol, i: integer;
begin
  if FGameOver then
    Exit;

  if not FAnimation.Done then
  begin
    Animation(MIGravity.Checked);
    Exit;
  end;

  if (not FPlayers[FALSE]) and (not FPlayers[TRUE]) then
  (* Human versus human mode, no need to continue. *)
    Exit;

  if not FWaiting then
    if (not FSideToMove) and FPlayers[FALSE]
    or (FSideToMove) and FPlayers[TRUE] then
    begin
      AskEngineToPlay(FLevel, FHistory);
      FWaiting := TRUE;
    end;

  FEngineOutput.Clear;
  if FProcess.Running then
    repeat
      LDone := TRUE;
      ReadOutput(FProcess);
    until LDone (* No more output *)
  else
  begin
    (* Shouldn't happen, unless Velena stopped to work. *)
    LogLn('** ' + {$I %LINE%} + ' Engine is not running.');
    ShowMessage('Velena stopped unexpectedly.');
    MIHumanVsHuman.Click;
    MIComputerWhite.Enabled := FALSE;
    MIComputerBlack.Enabled := FALSE;
  end;

  if FEngineOutput.Count > 0 then
  begin
    for i := 0 to Pred(FEngineOutput.Count) do
      LogLn('<< ' + FEngineOutput[i]);

    if FExpr.Exec(FEngineOutput.Text) then
    begin
      FWaiting := FALSE;
      LCol := StrToInt(FExpr.Match[1]);
      FRow := InsertDisk(FGrid, FSideToMove, LCol);
      FHistory := FHistory + IntToStr(LCol);
      StartAnimation(LCol);
    end;
  end;
end;

procedure TForm1.AskEngineToPlay(const ALevel: char; const AHistory: string);
var
  LCommand: string;
begin
  if FProcess.Running then
  begin
    (* Ask Velena to play. *)
    LCommand := Format('%s%s0' + LineEnding, [ALevel, AHistory]);
    FProcess.Input.Write(LCommand[1], Length(LCommand));
    LogLn('>> ' + Trim(LCommand));
  end else
    LogLn('** ' + {$I %LINE%} + ' Engine is not running.');
end;

end.

