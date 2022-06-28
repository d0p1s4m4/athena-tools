program aas;
{$IFDEF FPC}{$MODE OBJFPC}{H$H+}{$ENDIF}

uses
    Sysutils;

type
    ObjectFile = File of UInt32;
    TSym = record
        sym : string;
        address : UInt32;
    end;
    TInstr = record
        opcode : UInt8;
        ra : UInt8;
        rb : UInt8;
        rc : UInt8;
        shmat : UInt8;
        func : UInt8;
        offset : UInt32;
        imm : UInt16;
    end;

var
    Sym : Array of TSym = ();

procedure emit(var fd: ObjectFile; instr: UInt32);
begin
{$IFDEF ENDIAN_LITTLE}
    instr := SwapEndian(instr);
{$ENDIF}

    write(fd, instr);
end;

procedure emitIFormat(var fd: ObjectFile; opcode: UInt8; ra: UInt8; rb: UInt8; imm: UInt16);
var
    instr: UInt32;
begin
    instr := (imm shl 16);
    instr := instr or ((rb and $1F) shl 11);
    instr := instr or ((ra and $1F) shl 6);
    instr := instr or (opcode and $3F);
    emit(fd, instr);
end;

procedure emitRFormat(var fd: ObjectFile; opcode: UInt8; ra: UInt8; rb: UInt8; rc: UInt8; shmat: UInt8; func: UInt8);
var
    instr: UInt32;
begin
    instr := (func and $3F) shl 26;
    instr := instr or ((shmat and $1F) shl 21);
    instr := instr or ((rc and $1F) shl 16);
    instr := instr or ((rb and $1F) shl 11);
    instr := instr or ((ra and $1F) shl 6);
    instr := instr or (opcode and $3F);
    emit(fd, instr);
end;

procedure emitJFormat(var fd: ObjectFile; opcode: UInt8; ra: UInt8; offset: UInt32);
var
    instr: UInt32;
begin
    instr := (offset and $1FFFFF) shl 11;
    instr := instr or ((ra and $1F) shl 6);
    instr := instr or (opcode and $3F);
    emit(fd, instr);
end;

procedure emitLSFormat(var fd: ObjectFile; opcode: UInt8; ra: UInt8; rb: UInt8; width: UInt8; offset: UInt16);
var
    instr: UInt32;
begin
    instr := (offset and $3FFF) shl 18;
    instr := instr or ((width and $3) shl 16);
    instr := instr or ((rb and $1F) shl 11);
    instr := instr or ((ra and $1F) shl 6);
    instr := instr or (opcode and $3F);
end;

procedure run_pass(var sourceFd: TextFile; var binaryFd: ObjectFile; passNum: integer);
var
    line: String;
begin
    reset(sourceFd);
    if passNum = 2 then rewrite(binaryFd);

    while not eof(sourceFd) do
    begin
        readLn(sourceFd, line);
        line := Trim(line);
        if (Length(line) <> 0) and (line[1] <> ';') and (line[1] <> '#') then
        begin
            writeLn(line);
            if passNum = 2 then
            begin
                emitIFormat(binaryFd, %000001, 1, 0, 56);
                emitRFormat(binaryFd, 0, 0, 0, 0, 0, 0);
                emitJFormat(binaryFd, %001101, 0, 123456);
            end;
        end;
    end;
end;

procedure assemble(sourceFile: String);
var
    tempSource : String;
    outFile: String;
    sourceFd: TextFile;
    binaryFd: ObjectFile;
begin
    outFile := sourceFile;
    outFile[Length(outFile)] := 'o';
    (* if source file and with .S then preprocess *)
    if sourceFile[Length(sourceFile)] = 'S' then
    begin
        tempSource := GetTempFileName();
        (* TODO: pass flags to cpp *)
        executeprocess('/usr/bin/cpp', ['-o', tempSource, sourceFile]);
        sourceFile := tempSource
    end;

    writeLn(sourceFile);
    writeLn(outFile);

    AssignFile(sourceFd, sourceFile);
    AssignFile(binaryFd, outFile);

    try
        (* first pass *)
        run_pass(sourceFd, binaryFd, 1);

        (* second pass *)
        run_pass(sourceFd, binaryFd, 2);

        CloseFile(sourceFd);
        CloseFile(binaryFd);
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
        assemble(paramStr(idx));
    end;
end.