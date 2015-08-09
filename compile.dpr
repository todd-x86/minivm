program compile;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  uCompiler in 'uCompiler.pas';

var
  C: TCompiler;

begin
  C := TCompiler.Create;
  C.LoadFromFile(ParamStr(1));
  C.SaveToFile(ParamStr(2));

  C.Compile;

  C.Free;
end.
