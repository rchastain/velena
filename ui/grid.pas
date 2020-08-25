
unit grid;

interface

const
  CEmpty  =  0;
  CDisc   =  1;
  CBorder =  2;
  CBlack  = -1;
  CWhite  =  1;

type
  TDisk = CBlack..CBorder;
  TGrid = array[1 - 1..7 + 1, 1 - 1..6 + 1] of TDisk;
  TLine = record
            x1, y1, x2, y2, dx, dy: integer;
          end;

procedure EmptyGrid(var g: TGrid);
procedure CopyGrid(var g: TGrid; const h: TGrid);
function InsertDisk(var g: TGrid; const d: TDisk; const x: integer): boolean;
function IsEmpty(const g: TGrid): boolean;
function IsFull(const g: TGrid): boolean;
function GridToText(const g: TGrid): string;
function LineValue(const d: TDisk; const l: TLine; const g: TGrid): integer;
function GridValue(const d: TDisk; const g: TGrid): integer;

implementation

const
  CLines: array[0..24] of TLine = (
    (x1: 1; y1: 1; x2: 7; y2: 1; dx: 1; dy: 0),
    (x1: 1; y1: 2; x2: 7; y2: 2; dx: 1; dy: 0),
    (x1: 1; y1: 3; x2: 7; y2: 3; dx: 1; dy: 0),
    (x1: 1; y1: 4; x2: 7; y2: 4; dx: 1; dy: 0),
    (x1: 1; y1: 5; x2: 7; y2: 5; dx: 1; dy: 0),
    (x1: 1; y1: 6; x2: 7; y2: 6; dx: 1; dy: 0),
    (x1: 1; y1: 1; x2: 1; y2: 6; dx: 0; dy: 1),
    (x1: 2; y1: 1; x2: 2; y2: 6; dx: 0; dy: 1),
    (x1: 3; y1: 1; x2: 3; y2: 6; dx: 0; dy: 1),
    (x1: 4; y1: 1; x2: 4; y2: 6; dx: 0; dy: 1),
    (x1: 5; y1: 1; x2: 5; y2: 6; dx: 0; dy: 1),
    (x1: 6; y1: 1; x2: 6; y2: 6; dx: 0; dy: 1),
    (x1: 7; y1: 1; x2: 7; y2: 6; dx: 0; dy: 1),
    (x1: 1; y1: 1; x2: 7; y2: 6; dx: 1; dy: 1),
    (x1: 1; y1: 2; x2: 6; y2: 6; dx: 1; dy: 1),
    (x1: 1; y1: 3; x2: 5; y2: 6; dx: 1; dy: 1),
    (x1: 1; y1: 4; x2: 5; y2: 1; dx: 1; dy:-1),
    (x1: 1; y1: 5; x2: 6; y2: 1; dx: 1; dy:-1),
    (x1: 1; y1: 6; x2: 7; y2: 1; dx: 1; dy:-1),
    (x1: 7; y1: 1; x2: 7; y2: 6; dx:-1; dy: 1),
    (x1: 7; y1: 2; x2: 6; y2: 6; dx:-1; dy: 1),
    (x1: 7; y1: 3; x2: 5; y2: 6; dx:-1; dy: 1),
    (x1: 7; y1: 4; x2: 5; y2: 1; dx:-1; dy:-1),
    (x1: 7; y1: 5; x2: 6; y2: 1; dx:-1; dy:-1),
    (x1: 7; y1: 6; x2: 7; y2: 1; dx:-1; dy:-1)
  );

procedure EmptyGrid(var g: TGrid);
var
  x, y: integer;
begin
  for x := 1 - 1 to 7 + 1 do
    for y := 1 - 1 to 6 + 1 do
      g[x, y] := CBorder;

  for x := 1 to 7 do
    for y := 1 to 6 do
      g[x, y] := CEmpty;
end;

procedure CopyGrid(var g: TGrid; const h: TGrid);
var
  x, y: integer;
begin
  for x := 1 to 7 do
    for y := 1 to 6 do
      g[x, y] := h[x, y];
end;

function InsertDisk(var g: TGrid; const d: TDisk; const x: integer): boolean;
var
  y: integer;
begin
  y := 6;
  result := (x >= 1) and (x <= 7) and (g[x, y] = CEmpty);
  if result then
  begin
    while (y > 1) and (g[x, y - 1] = CEmpty) do
      Dec(y);
    g[x, y] := d;
  end;
end;

function IsEmpty(const g: TGrid): boolean;
var
  x: integer;
begin
  result := true;
  for x := 1 to 7 do
    if g[x, 1] <> CEmpty then
    begin
      result := FALSE;
      break;
    end;
end;

function IsFull(const g: TGrid): boolean;
var
  x: integer;
begin
  result := true;
  for x := 1 to 7 do
    if g[x, 6] = CEmpty then
    begin
      result := FALSE;
      break;
    end;
end;

function GridToText(const g: TGrid): string;
const
  CSymbol: array[TDisk] of char = ('O', '.', 'X', '#');
  CCanvas =
  '#################'#13#10 +
  '# x x x x x x x #'#13#10 +
  '# x x x x x x x #'#13#10 +
  '# x x x x x x x #'#13#10 +
  '# x x x x x x x #'#13#10 +
  '# x x x x x x x #'#13#10 +
  '# x x x x x x x #'#13#10 +
  '#################';
var
  x, y, i: integer;
begin
  result := CCanvas;
  for y := 6 downto 1 do
    for x := 1 to 7 do
    begin
      i := 1 + 19 * (6 - y + 1) + 2 * x;
      result[i] := CSymbol[g[x, y]];
    end;
end;

function LineValue(const d: TDisk; const l: TLine; const g: TGrid): integer;
var
  x, y, n, o, a, b: integer;
begin
  x := l.x1;
  y := l.y1;
  n := 0;
  o := 0;
  a := 0;
  b := 0;
  
  repeat
    if g[x,y] = d then
      Inc(n)
    else
      n := 0;
    if (g[x, y] = d) or (g[x, y] = CEmpty) then
      Inc(o)
    else
      o := 0;
    if n > a then
      a := n;
    if o > b then
      b := o;
    Inc(x, l.dx);
    Inc(y, l.dy);
  until g[x, y] = CBorder;
  
  if b >= 4 then
    result := a
  else
    result := 0;
end;

function GridValue(const d: TDisk; const g: TGrid): integer;
var
  i, j: integer;
begin
  result := 0;
  for i := 0 to 24 do
  begin
    j := LineValue(d, CLines[i], g);
    if j > result then result := j;
  end;
end;

end.