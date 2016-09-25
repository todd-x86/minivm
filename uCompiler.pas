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
    constructor Create;
  end;

  TCompiler = class(TObject)
  private
    FLexer: TLexer;
    FInFile: Text;
    FLine: Integer;
    FOutFile: TFileStream;
    FStrTable: TStringList;
    FDLLTable: TObjectList;
    FSlotTable: TStringList;
    procedure ParseLine;
    procedure ParseString;
    procedure ParseToken(const T: Byte; const Err: String);
    procedure CompileLine;
    procedure AddDLLEntry(const Fn: TFilename; const Func: String);
    procedure Error(const Str: String);
    procedure CompileAssignment;
    procedure CompileVoidFunc;
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

constructor TDLLEntry.Create;
begin
  inherited Create;
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
  FDLLTable := TObjectList.Create(true);
  FLexer := TLexer.Create;
  FSlotTable := TStringList.Create;

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
    FLexer.SetLine(ln);
    CompileLine;
  end;

  CloseFile(FInFile);
  FOutFile.Free;
end;

procedure TCompiler.ParseLine;
var t: Byte;
    // DLL-related vars
    id, dllFile: String;

begin
  Inc(FLine);

  // Parse strings and DLL entries
  t := FLexer.Next;
  if t = T_DIRECTIVE then begin
    // Tokens: T_DEF, T_IDENT, T_LPAR, {idents}, T_RPAR, T_COLON, T_STRING, T_EOL
    ParseToken(T_DEF, '"def" expected');
    ParseToken(T_IDENT, 'Identifier expected');
    id := FLexer.Value;
    t := FLexer.Next;
    if t <> T_LPAR then Error('"(" expected');
    t := FLexer.Next;
    if (t <> T_RPAR) then begin
      t := FLexer.Next;
      if t = T_IDENT then begin
        // Parse ',' and IDENT | MULTIARGS
        t := FLexer.Next;
        while t = T_COMMA do begin
          ParseToken(T_IDENT, 'Identifier expected');
          t := FLexer.Next;
        end;
      end else if t = T_MULTIARG then begin
        // TODO: Store multiargs in function reference
      end else Error('Expected identifier or multiargs');
    end;
    ParseToken(T_RPAR, '")" expected');
    ParseToken(T_COLON, '":" expected');
    ParseToken(T_STRING, 'String expected');
    dllFile := Lowercase(Copy(FLexer.Value, 2, Length(FLexer.Value)-2));
    ParseToken(T_EOL, 'End-of-line expected');

    AddDLLEntry(dllFile, id);
  end else begin
    // Find strings and store them in the string table
    while (t <> T_EOL) do begin
      if t = T_STRING then ParseString;
      t := FLexer.Next;
    end;
  end;
end;

procedure TCompiler.AddDLLEntry(const Fn: TFilename; const Func: String);
var j: Integer;
    dllEnt: TDLLEntry;
begin
  // TODO: Store in hashmap for fast lookup
  for j := 0 to FDLLTable.Count-1 do begin
    if CompareText((FDLLTable[j] as TDLLEntry).DLL, Fn) = 0 then begin
      dllEnt := (FDLLTable[j] as TDLLEntry);
      if dllEnt.Funcs.IndexOf(Func) < 0 then
        dllEnt.Funcs.Add(Func);
      Exit;
    end;
  end;
  dllEnt := TDLLEntry.Create;
  dllEnt.DLL := Fn;
  dllEnt.Funcs.Add(Func);
  FDLLTable.Add(dllEnt);
end;

procedure TCompiler.ParseToken(const T: Byte; const Err: String);
begin
  if FLexer.Next <> T then Error(Err);
end;

procedure TCompiler.ParseString;
var s, value: String;
    j: Integer;
begin
  j := 2;
  value := FLexer.Value;
  while j < Length(value) do begin
    if value[j] = '\' then begin
      Inc(j);
      if j >= Length(value) then Error('String literal does not have ending quote');
      case value[j] of
        'r': s := s + #13;
        'n': s := s + #10;
        't': s := s + #9;
        '0': s := s + #0;
        '"': s := s + '"';
        else s := s + value[j];
      end;
    end else s := s + value[j];
    Inc(j);
  end;

  // String index corresponds to sequence in source
  FStrTable.Add(s);
end;

procedure TCompiler.Error(const Str: String);
begin
  Writeln(Format('ERROR (line %d): %s', [FLine, Str]));
  Halt;
end;

procedure TCompiler.CompileLine;
var t: Byte;
begin
  Inc(FLine);

  // Possible statements:
  {
    @.... -> [skip]
    x = "..."
    x = func(arg1, arg2, ...)
    func(arg1, arg2, ...)
  }
  t := FLexer.Next;
  case t of
    T_DIRECTIVE: Exit;
    T_IDENT:
    begin
      if FLexer.Peek = T_EQU then
        CompileAssignment
      else if FLexer.Peek = T_LPAR then
        CompileVoidFunc
      else Error('Unknown statement');
    end;
    T_EOL: Exit;
    else Error('Unknown statement');
  end;
end;

procedure TCompiler.CompileAssignment;
var t: Byte;
    id: String;
begin
  // TODO: Add some serious optimizations - implement as parse trees
  FLexer.Next;
  t := FLexer.Next;
  if t = T_IDENT then begin
    id := FLexer.Value;
    t := FLexer.Next;
    if t = T_LPAR then begin
      // Assign a function call's result to a slot
      {
        Emit code:
        push_slot <x> | push_string <x:addr>
        ...
        call <func>
        store_result <id>
      }
    end else if t = T_EOL then begin
      // Assign a slot to a slot
      Error('This feature is not yet implemented');
    end else Error('Unknown rvalue');
  end else if t = T_STRING then begin
    // Assign a string to a slot
    Error('This feature is not yet implemented');
  end else if t = T_INT then begin
    // Assign an int to a slot
    Error('This feature is not yet implemented');
  end else Error('Cannot parse rvalue');
end;

procedure TCompiler.CompileVoidFunc;
begin

end;

end.
