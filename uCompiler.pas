unit uCompiler;

interface

uses Classes, Contnrs, SysUtils;

type
  TLexer = class(TObject)
  private
    FLine: String;
    FPos, FLast: Integer;
    function NextIdent: Byte;
    function NextNum: Byte;
    function NextSym: Byte;
  public
    constructor Create;
    procedure SetLine(const Line: String);
    function Peek: Byte;
    function Next: Byte;
    function Value: String;
  end;

  TDLLEntry = class(TObject)
  public
    DLL: String;
    Funcs: TStringList;
    constructor Create(const Fn: String);
  end;

  TCompiler = class(TObject)
  private
    FLexer: TLexer;
    FInFile: Text;
    FLine: Integer;
    FOutFile: TFileStream;
    FStrTable: TStringList;
    FDLLTable: TObjectList;
    procedure ParseLine;
    procedure CompileLine;
  public
    constructor Create;
    procedure LoadFromFile(const Fn: TFilename);
    procedure SaveToFile(const Fn: TFilename);
    procedure Compile;
  end;

implementation

const
  T_DIRECTIVE = 0;  // @
  T_DEF = 1;
  T_IDENT = 2;
  T_LPAR = 3;
  T_RPAR = 4;
  T_MULTIARG = 5; // ...
  T_COLON = 6;
  T_STRING = 7;
  T_EQU = 8;
  T_COMMA = 9;
  T_EOL = 10;
  T_UNKNOWN = 11;
  T_INT = 12;
  T_PERIOD = 13;

constructor TLexer.Create;
begin
  inherited Create;
end;

procedure TLexer.SetLine(const Line: String);
begin
  FLine := Line;
  FPos := 1;
  FLast := 1;
end;

function TLexer.Peek: Byte;
var tmpPos, tmpLast: Integer;
begin
  tmpPos := FPos;
  tmpLast := FLast;

  Result := Next;

  FLast := tmpLast;
  FPos := tmpPos;
end;

function TLexer.Next: Byte;
begin
  // Skip whitespace
  while (FPos <= Length(FLine)) and (FLine[FPos] in [#13,#10,#9,' ']) do Inc(FPos);

  FLast := FPos;
  // End of line check
  if FPos > Length(FLine) then begin
    Result := T_EOL;
    Exit;
  end;

  // Read token
  if FLine[FPos] in ['A'..'Z','a'..'z','_'] then
    Result := NextIdent
  else if FLine[FPos] in ['0'..'9'] then
    Result := NextNum
  else Result := NextSym;
end;

function TLexer.NextIdent: Byte;
var tmp: String;
begin
  while (FPos <= Length(FLine)) and (FLine[FPos] in ['A'..'Z','a'..'z','_','0'..'9']) do
    Inc(FPos);
  tmp := Lowercase(Value);
  if tmp = 'def' then Result := T_DEF
  else Result := T_IDENT;
end;

function TLexer.NextNum: Byte;
begin
  while (FPos <= Length(FLine)) and (FLine[FPos] in ['0'..'9']) do
    Inc(FPos);
  Result := T_INT;
end;

function TLexer.NextSym: Byte;
var tmpPos: Integer;
begin
  case FLine[FPos] of
    '@':
    begin
      Result := T_DIRECTIVE;
      Inc(FPos);
    end;
    '.':
    begin
      tmpPos := FPos;
      while (FPos <= Length(FLine)) and (FLine[FPos] = '.') do
        Inc(FPos);
      if FPos-tmpPos = 3 then begin
        Result := T_MULTIARG;
      end else begin
        FPos := tmpPos+1;
        Result := T_PERIOD;
      end;
    end;
    '"':
    begin
      Inc(FPos);
      while (FPos <= Length(FLine)) and (FLine[FPos] <> '"') do begin
        if FLine[FPos] = '\' then Inc(FPos);
        Inc(FPos);
      end;
      if FPos > Length(FLine) then begin
        Result := T_UNKNOWN;
      end else begin
        Inc(FPos);
        Result := T_STRING;
      end;
    end;
    ':':
    begin
      Result := T_COLON;
      Inc(FPos);
    end;
    '(':
    begin
      Result := T_LPAR;
      Inc(FPos);
    end;
    ')':
    begin
      Result := T_RPAR;
      Inc(FPos);
    end;
    ',':
    begin
      Result := T_COMMA;
      Inc(FPos);
    end;
    '=':
    begin
      Result := T_EQU;
      Inc(FPos);
    end;
    else
    begin
      Result := T_UNKNOWN;
      Inc(FPos);
    end;
  end;
end;

function TLexer.Value: String;
begin
  Result := Copy(FLine, FLast, FPos-FLast);
end;

{ DLL Entry }

constructor TDLLEntry.Create(const Fn: String);
begin
  inherited Create;
  DLL := Lowercase(Fn);
  Funcs := TStringList.Create;
end;

{ Compiler }

constructor TCompiler.Create;
begin
  inherited Create;
end;

procedure TCompiler.LoadFromFile(const Fn: TFilename);
begin
  AssignFile(FInFile, Fn);
  Reset(FInFile);
end;

procedure TCompiler.SaveToFile(const Fn: TFilename);
begin
  FOutFile := TFileStream.Create(Fn, fmCreate or fmOpenWrite);
end;

procedure TCompiler.Compile;
var ln: String;
begin
  FStrTable := TStringList.Create;

  FLexer := TLexer.Create;

  FLine := 0;
  // First pass - read DLLs and strings
  while not Eof(FInFile) do begin
    Readln(FInFile, ln);
    FLexer.SetLine(ln);
    ParseLine;
  end;
  Reset(FInFile);

  FLine := 0;

  // Second pass - compilation
  while not Eof(FInFile) do begin
    Readln(FInFile, ln);
    CompileLine;
  end;

  CloseFile(FInFile);
  FOutFile.Free;
end;

procedure TCompiler.ParseLine;
var t: Byte;
begin
  t := FLexer.Next;

end;

procedure TCompiler.CompileLine;
begin

end;

end.
