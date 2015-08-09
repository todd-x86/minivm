program minivm;

{$APPTYPE CONSOLE}

uses
  Windows, Classes, SysUtils;

type
  TOpFunction = procedure;

var
 fs: TFileStream;
 res: Cardinal;
 ops, ptr_op, ptr_dll, ptr_str, ptr_opEnd: PByte;
 sz: Cardinal;
 dllOffset, opOffset: Cardinal;
 dllHnd: Cardinal;
 dllFunc: TList;
 slots: array[0..255] of Byte;
 OpTable: array[0..3] of TOpFunction;
 stack: array[0..1023] of Pointer;
 stackSz, stackIndex: Integer;
 oldEbp, oldEsp: Pointer;

procedure AddStack(p: Pointer);
begin
  stack[stackIndex] := p;
  Inc(stackIndex);
end;

// Opcodes

procedure Op_PushString;
begin
  Inc(ptr_op);
  // Add string virtual address
  AddStack(Pointer(Cardinal(ptr_str)+PWord(ptr_op)^));
  Inc(ptr_op, sizeof(Word));
end;

procedure Op_Call;
var bPtr, sPtr: Pointer;
begin
  {
    NOTES:
      ESP < EBP
      ESP++ when pop
      ESP-- when push
  }
  Inc(ptr_op);

  // Function name on top of stack
  stack[0] :=  dllFunc[PWord(ptr_op)^];

  // For old EBP + ESP reg preservation
  AddStack(Pointer(0));
  AddStack(Pointer(0));

  Inc(ptr_op, sizeof(Word));

  sPtr := @stack;
  bPtr := Pointer(Cardinal(@stack) + (Sizeof(Pointer)*(stackSz-1)));

  // Push all operands from internal stack onto function stack
  asm
    // Preserve old EBP + ESP on top of stack ptr
    //mov [bPtr+4], ebp
    //mov [bPtr+8], esp
    mov [oldEbp], ebp
    mov [oldEsp], esp

    // Set stack pointer + base pointer
    mov ecx, sPtr
    mov edx, bPtr

    mov esp, ecx
    mov ebp, edx

    // Move function call from top of stack to [eax]
    pop eax
    call eax

    // Store result
    mov [res], eax

    mov esp, [oldEsp]
    mov ebp, [oldEbp]
  end;
  stackIndex := 1;
end;

procedure Op_StoreResult;
begin
  Inc(ptr_op);
  slots[ptr_op^] := res;
  Inc(ptr_op);
end;

procedure Op_PushSlot;
begin
  Inc(ptr_op);
  AddStack(Pointer(slots[ptr_op^]));
  Inc(ptr_op);
end;

// Opcodes end

function ReadString(var ptr: PByte): PChar;
begin
  Result := PChar(ptr);
  while ptr^ > 0 do Inc(ptr);
  Inc(ptr);
end;

begin
  fs := TFileStream.Create('simple2.b', fmOpenRead);
  sz := fs.Size-(sizeof(Cardinal)*2);
  ops := AllocMem(sz);
  fs.Read(ops^, sz);
  fs.Read(dllOffset, sizeof(Cardinal));
  fs.Read(opOffset, sizeof(Cardinal));
  fs.Free;

  stackSz := 1024;
  stackIndex := 1;

  // Initialize op table
  OpTable[0] := Op_PushString;
  OpTable[1] := Op_Call;
  OpTable[2] := Op_StoreResult;
  OpTable[3] := Op_PushSlot;

  ptr_str := ops;
  ptr_dll := PByte(Cardinal(ops) + dllOffset);
  ptr_op := PByte(Cardinal(ops) + opOffset);
  ptr_opEnd := PByte(Cardinal(ops) + sz);

  // NOTE: String table remains in memory untouched
  // Register DLL functions
  dllFunc := TList.Create;
  while Cardinal(ptr_dll) < Cardinal(ptr_op) do begin
    if ptr_dll^ = 1 then begin
      // LoadLibrary
      Inc(ptr_dll);
      dllHnd := LoadLibrary(ReadString(ptr_dll));
    end else if ptr_dll^ = 2 then begin
      // GetProcAddress
      Inc(ptr_dll);
      dllFunc.Add(GetProcAddress(dllHnd, ReadString(ptr_dll)));
    end else MessageBox(0, 'Invalid DLL table opcode', 'VM error', 0);
  end;

  // Opcode execution
  while Cardinal(ptr_op) < Cardinal(ptr_opEnd)-1 do begin
    Writeln(ptr_op^);
    OpTable[ptr_op^]();
  end;
end.
