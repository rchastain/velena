{
	Lightweight Translation Manager

  Author: Patrick Lampke
  Version: 1.0
  Date: 31.12.2018
  License: WTFPL

  http://www.patrick-lampke.de/index.php?page=lmt

  Translations are based on ini files

  Keys starting with "$" are ignored by the parser. They are for properties of the language:
  $name       Friendly name of the language
  $inherits   list of parent languages (seperated by comma)

  Example ini file:


  [en-us]
  $name=English (United States)
  foo.name=Example program
  foo.close=Close
  foo.color=Color

  [en-gb]
  $name=English (United Kingdom)
  $inherits=en-us
  foo.color=Colour

  [de-de]
  $name=Deutsch (Deutschland)
  foo.name=Beispielprogramm
  foo.close=Schließen
  foo.color=Farbe

  [de-ch]
  $name=Deutsch (Schweiz)
  $inherits=de-de
  foo.close=Schliessen
}
unit LightweightTranslationManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

type
  TLTMEntry = class
    private
      FKeyName: AnsiString;
      FTranslated: AnsiString;
    public
      constructor Create(AKeyName, ATranslated: AnsiString); reintroduce; virtual;
      property KeyName: AnsiString read FKeyName;
      property Translated: AnsiString read FTranslated write FTranslated;
  end;

  TLTMEntries = array of TLTMEntry;

  TLightweightTranslationManager = class
    private
			FIniFile: AnsiString;
      FLanguage: AnsiString;
      FLanguageName: AnsiString;
      FItems: TLTMEntries;
      FLoaded: TStringList; // only for internal use to prevent reciprocally heredity
      procedure SetIniFile(s: AnsiString);
      procedure SetLanguage(s: AnsiString);
    protected
      procedure LoadLanguage(ini: TIniFile; Section: AnsiString); virtual; // only for internal use. Use Load() or LoadWithoutClear() instead.
      procedure LoadWithoutClear; virtual;
      procedure Load; virtual;
      procedure Clear; virtual;
      function Put(AKeyName, ATranslated: AnsiString): TLTMEntry; virtual;
    public
      constructor Create; virtual;
      constructor Create(AIniFile, ALanguage: AnsiString); virtual;
      destructor Destroy; override;
      function Get(AKeyName: AnsiString): TLTMEntry; virtual;
      function GetString(AKeyName: AnsiString; DefaultValue: AnsiString = ''): AnsiString; virtual;
      property IniFile: AnsiString read FIniFile write SetIniFile;
      property Language: AnsiString read FLanguage write SetLanguage;
      property LanguageName: AnsiString read FLanguageName;
  end;

  TLTM = TLightweightTranslationManager;

var
  TransMgr: TLightweightTranslationManager;

implementation

{ TLTMEntry }

constructor TLTMEntry.Create(AKeyName, ATranslated: AnsiString);
begin
  FKeyName := AKeyName;
  FTranslated := ATranslated;
end;

{ TLightweightTranslationManager }

constructor TLightweightTranslationManager.Create;
begin
	SetLength(FItems, 0);
  FIniFile := '';
  FLanguage := '';
end;

constructor TLightweightTranslationManager.Create(AIniFile, ALanguage: AnsiString);
begin
	SetLength(FItems, 0);
  FIniFile := AIniFile;
  FLanguage := ALanguage;
end;

destructor TLightweightTranslationManager.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TLightweightTranslationManager.SetIniFile(s: AnsiString);
begin
	FIniFile := s;
  Load;
end;

procedure TLightweightTranslationManager.SetLanguage(s: AnsiString);
begin
  FLanguage := s;
  Load;
end;

procedure TLightweightTranslationManager.LoadLanguage(ini: TIniFile; Section: AnsiString);
var
  sl: TStrings;
  s: AnsiString;
  i: Integer; // useless, only for TStringList.Find()
begin
  if ini <> nil then begin
    // add this language to the already loaded languages (to prevent reciprocally heredity)
    FLoaded.Add(LowerCase(Section));
		// inheritage
    for s in ini.ReadString(Section, '$inherits', '').Split([',']) do begin
      if (Trim(s) <> '') and (not FLoaded.Find(LowerCase(s), i)) then begin
        LoadLanguage(ini, s);
      end;
    end;
    // load content
		sl := TStringList.Create;
		ini.ReadSection(Section, sl);
    for s in sl do begin
      if (Length(s) > 0) and (s[1] <> '$') then begin
				Put(s, ini.ReadString(Section, s, s));
      end;
    end;
    FreeAndNil(sl);
  end;
end;

procedure TLightweightTranslationManager.LoadWithoutClear;
var
  ini: TIniFile;
begin
  if Language <> '' then begin
    ini := TIniFile.Create(IniFile);
	  try
      FLoaded := TStringList.Create;
      FLoaded.Sorted := true;
		  LoadLanguage(ini, Language);
      FLanguageName := ini.ReadString(Language, '$name', Language);
    finally
      FreeAndNil(FLoaded);
      FreeAndNil(ini);
    end;
  end;
end;

procedure TLightweightTranslationManager.Load;
begin
  Clear;
  LoadWithoutClear;
end;

procedure TLightweightTranslationManager.Clear;
var
  i, len: Integer;
begin
	len := Length(FItems);
  if len > 0 then begin
    for i := len - 1 downto 0 do begin
      FItems[i].Free;
      FItems[i] := nil;
    end;
    SetLength(FItems, 0);
  end;
end;

function TLightweightTranslationManager.Put(AKeyName, ATranslated: AnsiString): TLTMEntry;
var
  e: TLTMEntry;
  i: Integer;
begin
	e := Get(AKeyName);
  if e <> nil then e.Translated := ATranslated
  else begin
    e := TLTMEntry.Create(AKeyName, ATranslated);
		i := Length(FItems);
    SetLength(FItems, i + 1);
    FItems[i] := e;
  end;
  Result := e;
end;

function TLightweightTranslationManager.Get(AKeyName: AnsiString): TLTMEntry;
var
  e: TLTMEntry;
begin
  Result := nil;
	for e in FItems do begin
    if (e <> nil) and (LowerCase(e.KeyName) = LowerCase(AKeyName)) then begin
      Result := e;
      Break;
    end;
  end;
end;

function TLightweightTranslationManager.GetString(AKeyName: AnsiString; DefaultValue: AnsiString = ''): AnsiString;
var
  e: TLTMEntry;
begin
	e := Get(AKeyName);
  if e <> nil then Result := e.Translated
  else if DefaultValue <> '' then Result := DefaultValue
  else Result := AKeyName;
end;

initialization
	TransMgr := TLightweightTranslationManager.Create;

finalization
	FreeAndNil(TransMgr);

end.

