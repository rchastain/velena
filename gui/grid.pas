
unit grid;

(* Connect Four grid *)

interface

uses
  SysUtils;

type
  TDisk = ansichar;
  
const
  CWhite = 'X';
  CBlack = 'O';
  CEmpty = '.';
  CNihil = '#';
  
type
  TGrid = array[0..8, 0..7] of TDisk;
  TLineIndex = 0..24;

procedure EmptyGrid(var AGrid: TGrid);
procedure CopyGrid(var AGrid: TGrid; const ASource: TGrid);
function InsertDisk(var AGrid: TGrid; const ADisk: TDisk; const x: integer): integer; overload;
function InsertDisk(var AGrid: TGrid; const AColor: boolean; const x: integer): integer; overload;
function IsEmpty(const AGrid: TGrid): boolean;
function IsFull(const AGrid: TGrid): boolean;
function GridToText(const AGrid: TGrid): string;
function LineValue(const ADisk: TDisk; const i: TLineIndex; const AGrid: TGrid): integer;
function GridValue(const ADisk: TDisk; const AGrid: TGrid): integer;

implementation

type
  TLine = record
    fx, fy, tx, ty, dx, dy: integer;
  end;
  
const
  CLines: array[TLineIndex] of TLine = (
    (fx: 1; fy: 1; tx: 7; ty: 1; dx: 1; dy: 0),
    (fx: 1; fy: 2; tx: 7; ty: 2; dx: 1; dy: 0),
    (fx: 1; fy: 3; tx: 7; ty: 3; dx: 1; dy: 0),
    (fx: 1; fy: 4; tx: 7; ty: 4; dx: 1; dy: 0),
    (fx: 1; fy: 5; tx: 7; ty: 5; dx: 1; dy: 0),
    (fx: 1; fy: 6; tx: 7; ty: 6; dx: 1; dy: 0),
    (fx: 1; fy: 1; tx: 1; ty: 6; dx: 0; dy: 1),
    (fx: 2; fy: 1; tx: 2; ty: 6; dx: 0; dy: 1),
    (fx: 3; fy: 1; tx: 3; ty: 6; dx: 0; dy: 1),
    (fx: 4; fy: 1; tx: 4; ty: 6; dx: 0; dy: 1),
    (fx: 5; fy: 1; tx: 5; ty: 6; dx: 0; dy: 1),
    (fx: 6; fy: 1; tx: 6; ty: 6; dx: 0; dy: 1),
    (fx: 7; fy: 1; tx: 7; ty: 6; dx: 0; dy: 1),
    (fx: 1; fy: 1; tx: 6; ty: 6; dx: 1; dy: 1),
    (fx: 1; fy: 2; tx: 5; ty: 6; dx: 1; dy: 1),
    (fx: 1; fy: 3; tx: 4; ty: 6; dx: 1; dy: 1),
    (fx: 1; fy: 4; tx: 4; ty: 1; dx: 1; dy:-1),
    (fx: 1; fy: 5; tx: 5; ty: 1; dx: 1; dy:-1),
    (fx: 1; fy: 6; tx: 6; ty: 1; dx: 1; dy:-1),
    (fx: 7; fy: 1; tx: 2; ty: 6; dx:-1; dy: 1),
    (fx: 7; fy: 2; tx: 3; ty: 6; dx:-1; dy: 1),
    (fx: 7; fy: 3; tx: 4; ty: 6; dx:-1; dy: 1),
    (fx: 7; fy: 4; tx: 4; ty: 1; dx:-1; dy:-1),
    (fx: 7; fy: 5; tx: 3; ty: 1; dx:-1; dy:-1),
    (fx: 7; fy: 6; tx: 2; ty: 1; dx:-1; dy:-1)
  );

procedure EmptyGrid(var AGrid: TGrid);
var
  x, y: integer;
begin
  for x := 0 to 8 do
    for y := 0 to 7 do
      AGrid[x, y] := CNihil;

  for x := 1 to 7 do
    for y := 1 to 6 do
      AGrid[x, y] := CEmpty;
end;

procedure CopyGrid(var AGrid: TGrid; const ASource: TGrid);
var
  x, y: integer;
begin
  for x := 1 to 7 do
    for y := 1 to 6 do
      AGrid[x, y] := ASource[x, y];
end;

function InsertDisk(var AGrid: TGrid; const ADisk: TDisk; const x: integer): integer;
var
  y: integer;
begin
  y := 6;
  if (x >= 1) and (x <= 7) and (AGrid[x, y] = CEmpty) then
  begin
    while (y > 1) and (AGrid[x, y - 1] = CEmpty) do
      Dec(y);
    AGrid[x, y] := ADisk;
    result := y;
  end else
    result := 0;
end;

function InsertDisk(var AGrid: TGrid; const AColor: boolean; const x: integer): integer;
begin
  if AColor then
    result := InsertDisk(AGrid, CBlack, x)
  else
    result := InsertDisk(AGrid, CWhite, x);
end;

function IsEmpty(const AGrid: TGrid): boolean;
var
  x: integer;
begin
  result := TRUE;
  for x := 1 to 7 do
    if AGrid[x, 1] <> CEmpty then
    begin
      result := FALSE;
      break;
    end;
end;

function IsFull(const AGrid: TGrid): boolean;
var
  x: integer;
begin
  result := TRUE;
  for x := 1 to 7 do
    if AGrid[x, 6] = CEmpty then
    begin
      result := FALSE;
      break;
    end;
end;

function GridToText(const AGrid: TGrid): string;
const
  CCanvas =
  '#################'+ LineEnding +
  '# %s %s %s %s %s %s %s #'+ LineEnding +
  '# %s %s %s %s %s %s %s #'+ LineEnding +
  '# %s %s %s %s %s %s %s #'+ LineEnding +
  '# %s %s %s %s %s %s %s #'+ LineEnding +
  '# %s %s %s %s %s %s %s #'+ LineEnding +
  '# %s %s %s %s %s %s %s #'+ LineEnding +
  '#################';
begin
  result := Format(CCanvas, [
    AGrid[1, 6], AGrid[2, 6], AGrid[3, 6], AGrid[4, 6], AGrid[5, 6], AGrid[6, 6], AGrid[7, 6],
    AGrid[1, 5], AGrid[2, 5], AGrid[3, 5], AGrid[4, 5], AGrid[5, 5], AGrid[6, 5], AGrid[7, 5],
    AGrid[1, 4], AGrid[2, 4], AGrid[3, 4], AGrid[4, 4], AGrid[5, 4], AGrid[6, 4], AGrid[7, 4],
    AGrid[1, 3], AGrid[2, 3], AGrid[3, 3], AGrid[4, 3], AGrid[5, 3], AGrid[6, 3], AGrid[7, 3],
    AGrid[1, 2], AGrid[2, 2], AGrid[3, 2], AGrid[4, 2], AGrid[5, 2], AGrid[6, 2], AGrid[7, 2],
    AGrid[1, 1], AGrid[2, 1], AGrid[3, 1], AGrid[4, 1], AGrid[5, 1], AGrid[6, 1], AGrid[7, 1]
  ]);
end;

function LineValue(const ADisk: TDisk; const i: TLineIndex; const AGrid: TGrid): integer;
var
  x, y, v, w, vmax, wmax: integer;
begin
  x := CLines[i].fx;
  y := CLines[i].fy;
  v := 0;
  w := 0;
  vmax := 0;
  wmax := 0;
  
  repeat
    if AGrid[x, y] = ADisk then
      Inc(v)
    else
      v := 0;
    if (AGrid[x, y] = ADisk) or (AGrid[x, y] = CEmpty) then
      Inc(w)
    else
      w := 0;
    
    if v > vmax then
      vmax := v;
    if w > wmax then
      wmax := w;
    
    Inc(x, CLines[i].dx);
    Inc(y, CLines[i].dy);
  until AGrid[x, y] = CNihil;
  
  if wmax >= 4 then
    result := vmax
  else
    result := 0;
end;

function GridValue(const ADisk: TDisk; const AGrid: TGrid): integer;
var
  i, v: integer;
begin
  result := 0;
  for i := Low(TLineIndex) to High(TLineIndex) do
  begin
    v := LineValue(ADisk, i, AGrid);
    if v > result then result := v;
  end;
end;

{$IFDEF DEBUG}
procedure CheckLine(const i: TLineIndex);
var
  x, y: integer;
begin
  x := CLines[i].fx;
  y := CLines[i].fy;
  repeat
    Inc(x, CLines[i].dx);
    Inc(y, CLines[i].dy);
  until (x = CLines[i].tx) and (y = CLines[i].ty);
end;

procedure CheckLines;
var
  i: integer;
begin
  Write('CheckLines... ');
  for i := Low(TLineIndex) to High(TLineIndex) do
    CheckLine(i);
  WriteLn('done');
end;

begin
  (*
  CheckLines;
  *)
{$ENDIF}
end.
