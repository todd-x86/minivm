program minivm;

{$APPTYPE CONSOLE}

uses
  Windows, SysUtils;

var
 MsgBox: Pointer;
 h: Cardinal;
 txt1, txt2: PChar;

begin
  { TODO -oUser -cConsole Main : Insert code here }
  h := LoadLibrary('user32.dll');
  MsgBox := GetProcAddress(h, 'MessageBoxA');
  txt2 := AllocMem(6);
  StrPCopy(txt2, 'World');
  txt1 := AllocMem(6);
  StrPCopy(txt1, 'Hello');
  asm
    push 2
    push txt1
    push txt2
    push 0
    call MsgBox
  end;
  FreeLibrary(h);
end.
