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
 slots: array[0..255] of Pointer;
 OpTable: array[0..3] of TOpFunction;
 stackSz, stackIndex: Integer;
 oldEbp, oldEsp: Pointer;
 stack: array[0..4095] of Pointer;

procedure AddStack(p: Pointer);
begin
  stack[stackIndex] := p;
  Dec(stackIndex);
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
  AddStack(dllFunc[PWord(ptr_op)^]);

  Inc(ptr_op, sizeof(Word));

  sPtr := Pointer(Cardinal(@stack) + ((stackIndex+1)*sizeof(Pointer)));
  bPtr := Pointer(Cardinal(@stack) + (Sizeof(Pointer)*(stackSz-1)));

  // Push all operands from internal stack onto function stack
  asm
    // Preserve old EBP + ESP on top of stack ptr
    mov [oldEbp], ebp
    mov [oldEsp], esp

    // Set stack pointer + base pointer
    mov ecx, sPtr
    mov edx, bPtr

    mov esp, ecx
    mov ebp, edx

    // Move function call from top of stack to [edx]
    pop edx
    call edx

    // Store result
    mov [res], eax

    mov esp, [oldEsp]
    mov ebp, [oldEbp]
  end;
  stackIndex := High(stack);
end;

procedure Op_StoreResult;
begin
  Inc(ptr_op);
  slots[ptr_op^] := Pointer(res);
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
  fs := TFileStream.Create(ParamStr(0), fmOpenRead or fmShareDenyNone);
  fs.Seek(-4, soEnd);
  fs.Read(sz, sizeof(Cardinal));

  fs.Seek(-Integer(sz), soEnd);

  // Ignore 3 offsets (DLL table offset, string table offset, file size offset)
  Dec(sz, 3*sizeof(Cardinal));

  ops := AllocMem(sz);
  fs.Read(ops^, sz);
  fs.Read(dllOffset, sizeof(Cardinal));
  fs.Read(opOffset, sizeof(Cardinal));
  fs.Free;

  stackIndex := High(stack)-1;

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
  while Cardinal(ptr_op) < Cardinal(ptr_opEnd) do begin
    OpTable[ptr_op^]();
  end;
end.
