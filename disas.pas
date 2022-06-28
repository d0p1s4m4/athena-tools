program adisas;
{$IFDEF FPC}{$MODE OBJFPC}{H$H+}{$ENDIF}

uses
    Sysutils;

const
    REGS: Array of String = (
        'zero', 'a0', 'a1', 'a2', 'a3', 'a4', 'a5', 'a6', 'a7', 'r9', 'r10',
        'r11', 'r12', 'r13', 'r14', 'r15', 'r16', 'r17', 'r18', 'r19', 'r20',
        'r21', 'r22', 'r23', 'r24', 'r25', 'k0', 'k1', 'sp', 'gp', 'fp', 'ra'
    );

procedure printIFormat(decodedInstr: String; ra: UInt8; rb: UInt8; imm: UInt16);
begin
    writeLn(decodedInstr, ' ', REGS[ra], ', ', REGS[rb], ', ', imm);
end;

procedure printRFormat(decodedInstr: String; ra: UInt8; rb: UInt8; rc: UInt8);
begin
    writeLn(decodedInstr, ' ', REGS[ra], ', ', REGS[rb], ', ', REGS[rc]);
end;

procedure printRShiftFormat(decodedInstr: String; ra: UInt8; rb: UInt8; shmat: UInt8);
begin
    writeLn(decodedInstr, ' ', REGS[ra], ', ', REGS[rb], ', ', shmat);
end;

procedure printJFormat(decodedInstr: String; ra: UInt8; joffset: UInt32);
begin
    writeLn(decodedInstr, ' ', REGS[ra], ', ', joffset);
end;

procedure decodeFunc(ra: UInt8; rb: UInt8; rc: UInt8; shamt: Uint8; func: UInt8);
begin
    case func of
        %000001: printRFormat('add', ra, rb, rc);
        %000010: printRFormat('addu', ra, rb, rc);
        %000101: printRFormat('and', ra, rb, rc);
        %000110: printRFormat('div', ra, rb, rc);
        %000111: printRFormat('divu', ra, rb, rc);
        %010011: printRFormat('mod', ra, rb, rc);
        %010100: printRFormat('modu', ra, rb, rc);
        %001000: writeLn('mult ', REGS[ra], ' ', REGS[rb], ' ', REGS[rc], ' ', REGS[shamt]);
        %001001: writeLn('multu ', REGS[ra], ' ', REGS[rb], ' ', REGS[rc], ' ', REGS[shamt]);
        %000000: writeLn('nop');
        %001010: printRFormat('nor', ra, rb, rc);
        %001011: printRFormat('or', ra, rb, rc);
        %001101: printRShiftFormat('ssl', ra, rb, shamt);
        %010000: printIFormat('sslr', ra, rb, rc);
        %001111: printRShiftFormat('sra', ra, rb, shamt);
        %010001: printRFormat('srar', ra, rb, rc);
        %001110: printRShiftFormat('srl', ra, rb, shamt);
        %010010: printRFormat('srlr', ra, rb, rc);
        %000011: printRFormat('sub', ra, rb, rc);
        %000100: printRFormat('subu', ra, rb, rc);
        %001100: printRFormat('xor', ra, rb, rc);
        %111111: writeLn('trap');
    else
        writeLn('???');
    end;
end;

procedure decodeLoadStore(opcode: Uint8; ra: Uint8; rb: Uint8; ssize: Uint8; offset: UInt16);
begin
    if (opcode = %001001) or (opcode = %001100) then write('l') else write('s');
    case ssize of
        %00: write('b');
        %01: write('h');
        %10: write('w');
    end;
    if opcode = %001100 then write('u');
    if (opcode = %001001) or (opcode = %001100) then
        writeLn(' ', REGS[ra], ', ', offset, '(', REGS[rb], ')')
    else
        writeLn(' ', offset, '(', REGS[ra], '), ', REGS[rb]);
end;

procedure decodeInstr(instr: UInt32);
var
    opcode: UInt8;
    func: UInt8;
    ra: UInt8;
    rb: UInt8;
    rc: UInt8;
    shmat: UInt8;
    imm: UInt16;
    joffset: UInt32;
    lssize: UInt8;
    lsoffset: UInt16;
begin
{$IFDEF ENDIAN_LITTLE}
    instr := SwapEndian(instr);
{$ENDIF}

    opcode := instr and $3F;
    func := (instr shr 26) and $3F;
    ra := (instr shr 6) and $1F;
    rb := (instr shr 11) and $1F;
    rc := (instr shr 16) and $1F;
    shmat := (instr shr 21) and $1F;
    imm := (instr shr 16) and $FFFF;
    joffset := (instr shr 11) and $1FFFFF;
    lssize := (instr shr 16) and $2;
    lsoffset := (instr shr 18);

    case opcode of
        %000000: decodeFunc(ra, rb, rc, shmat, func);
        %000001: printIFormat('addi', ra, rb, imm);
        %000010: printIFormat('addiu', ra, rb, imm);
        %000101: printIFormat('andi', ra, rb, imm);
        %001110:
        begin
            if rb <> 0 then
                printIFormat('beq', ra, rb, imm)
            else if ra <> 0 then
                writeLn('beqz r', ra, ', ', imm)
            else writeLn('b ', imm);
        end;
        %010001:
        begin
            if rb <> 0 then
                printIFormat('bgt', ra, rb, imm)
            else writeLn('bgtz ', REGS[ra], ', ', imm);
        end;
        %010000:
        begin
            if rb <> 0 then
                printIFormat('blt', ra, rb, imm)
            else writeLn('bltz ', REGS[ra], ', ', imm);
        end;
        %001111:
        begin
            if rb <> 0 then
                printIFormat('bne', ra, rb, imm)
            else writeLn('bnez ', REGS[ra], ', ', imm);
        end;
        %001101: printJFormat('call', ra, joffset);
        %001010: printJFormat('jmp', ra, joffset);
        %001001: decodeLoadStore(opcode, ra, rb, lssize, lsoffset);
        %001100: decodeLoadStore(opcode, ra, rb, lssize, lsoffset);
        %001011: writeLn('lih ', REGS[ra], ', ', imm);
        %000110: printIFormat('ori', ra, rb, imm);
        %001000: decodeLoadStore(opcode, ra, rb, lssize, lsoffset);
        %000011: printIFormat('subi', ra, rb, imm);
        %000100: printIFormat('subiu', ra, rb, imm);
        %000111: printIFormat('xori', ra, rb, imm);
    else
        writeLn('???');
    end;
end;

procedure disassemble(binaryFile: String);
var
    fd: File of UInt32;
    instr: UInt32;
    address: integer;
begin
    AssignFile(fd, binaryFile);

    try
        reset(fd);

        address := 0;
        while not eof(fd) do
        begin
            read(fd, instr);
            write('0x', IntToHex(address), ' ');
            decodeInstr(instr);
            address := address + 4;
        end;

        CloseFile(fd);
    except
        on E: EInOutError do
            writeLn('Error: ', E.Message);
    end;
end;

var
    idx: integer;
begin
    for idx := 1 to paramCount() do
    begin
        disassemble(paramStr(idx));
    end;
end.
