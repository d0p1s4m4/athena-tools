open Ast

let symtable = Hashtbl.create 512

let resolve_symbol_address sym =
  if Hashtbl.mem symtable sym then Hashtbl.find symtable sym
  else failwith "sym not found"

let resolve_absolute_to_relative_address addr pc = (addr - pc) / 4

let resolve_relative_address addr pc =
  match addr with
  | Raw raw -> raw
  | RLabel label ->
      resolve_absolute_to_relative_address (resolve_symbol_address label) pc

let resolve_instruction instr pc =
  match instr with
  | Add (ra, rb, rc) -> (pc + 4, [ Opcode.opcode_add ra rb rc ])
  | Addi (ra, rb, imm) -> (pc + 4, [ Opcode.opcode_addi ra rb imm ])
  | Addiu (ra, rb, imm) -> (pc + 4, [ Opcode.opcode_addiu ra rb imm ])
  | Addu (ra, rb, rc) -> (pc + 4, [ Opcode.opcode_addu ra rb rc ])
  | And (ra, rb, rc) -> (pc + 4, [ Opcode.opcode_and ra rb rc ])
  | Andi (ra, rb, imm) -> (pc + 4, [ Opcode.opcode_andi ra rb imm ])
  | Beq (ra, rb, addr) ->
      (pc + 4, [ Opcode.opcode_beq ra rb (resolve_relative_address addr pc) ])
  | Bge (ra, rb, addr) ->
      (pc + 4, [ Opcode.opcode_bge ra rb (resolve_relative_address addr pc) ])
  | Bgeu (ra, rb, addr) ->
      (pc + 4, [ Opcode.opcode_bgeu ra rb (resolve_relative_address addr pc) ])
  | Blt (ra, rb, addr) ->
      (pc + 4, [ Opcode.opcode_blt ra rb (resolve_relative_address addr pc) ])
  | Bltu (ra, rb, addr) ->
      (pc + 4, [ Opcode.opcode_bltu ra rb (resolve_relative_address addr pc) ])
  | Bne (ra, rb, addr) ->
      (pc + 4, [ Opcode.opcode_bne ra rb (resolve_relative_address addr pc) ])
  | Call (RegOffset (r, o)) -> (pc + 4, [ Opcode.opcode_call r o ])
  | Call (FLabel f) ->
      (pc + 4, [ Opcode.opcode_call 0 (resolve_symbol_address f) ])
  | Div (ra, rb, rc) -> (pc + 4, [ Opcode.opcode_div ra rb rc ])
  | Divu (ra, rb, rc) -> (pc + 4, [ Opcode.opcode_divu ra rb rc ])
  | Jmp (RegOffset (r, o)) -> (pc + 4, [ Opcode.opcode_jmp r o ])
  | Jmp (FLabel f) ->
      (pc + 4, [ Opcode.opcode_jmp 0 (resolve_symbol_address f) ])
  | La (ra, label) ->
      let addr = resolve_symbol_address label in
      if addr land 0xFFFF0000 == 0 then (pc + 4, [ Opcode.opcode_ori ra 0 addr ])
      else
        ( pc + 8,
          [
            Opcode.opcode_lih 27 (addr lsr 16);
            Opcode.opcode_ori ra 27 (addr land 0xFFFF);
          ] )
  | Lb (ra, RegOffset (r, o)) -> (pc + 4, [ Opcode.opcode_load ra r 0 o ])
  | Lbu (ra, RegOffset (r, o)) -> (pc + 4, [ Opcode.opcode_loadu ra r 0 o ])
  | Lh (ra, RegOffset (r, o)) -> (pc + 4, [ Opcode.opcode_load ra r 1 o ])
  | Lhu (ra, RegOffset (r, o)) -> (pc + 4, [ Opcode.opcode_loadu ra r 1 o ])
  | Lw (ra, RegOffset (r, o)) -> (pc + 4, [ Opcode.opcode_load ra r 2 o ])
  | Li (ra, raw) ->
      if raw land 0xFFFF0000 == 0 then (pc + 4, [ Opcode.opcode_ori ra 0 raw ])
      else
        ( pc + 8,
          [
            Opcode.opcode_lih 27 (raw lsr 16);
            Opcode.opcode_ori ra 27 (raw land 0xFFFF);
          ] )
  | Mod (ra, rb, rc) -> (pc + 4, [ Opcode.opcode_mod ra rb rc ])
  | Modu (ra, rb, rc) -> (pc + 4, [ Opcode.opcode_modu ra rb rc ])
  | Mult (ra, rb, rc, rd) -> (pc + 4, [ Opcode.opcode_mult ra rb rc rd ])
  | Multu (ra, rb, rc, rd) -> (pc + 4, [ Opcode.opcode_multu ra rb rc rd ])
  | Mvsrr (ra, sr) -> (pc + 4, [ Opcode.opcode_mvsrr ra sr ])
  | Mvsrw (sr, ra) -> (pc + 4, [ Opcode.opcode_mvsrw sr ra ])
  | Nor (ra, rb, rc) -> (pc + 4, [ Opcode.opcode_nor ra rb rc ])
  | Or (ra, rb, rc) -> (pc + 4, [ Opcode.opcode_or ra rb rc ])
  | Ori (ra, rb, imm) -> (pc + 4, [ Opcode.opcode_ori ra rb imm ])
  | Rett -> (pc + 4, [ Opcode.opcode_rett ])
  | Sb (RegOffset (r, o), ra) -> (pc + 4, [ Opcode.opcode_store ra r 0 o ])
  | Sh (RegOffset (r, o), ra) -> (pc + 4, [ Opcode.opcode_store ra r 1 o ])
  | Sll (ra, rb, shamt) -> (pc + 4, [ Opcode.opcode_sll ra rb shamt ])
  | Sllr (ra, rb, rc) -> (pc + 4, [ Opcode.opcode_sllr ra rb rc ])
  | Sra (ra, rb, shamt) -> (pc + 4, [ Opcode.opcode_sra ra rb shamt ])
  | Srar (ra, rb, rc) -> (pc + 4, [ Opcode.opcode_srar ra rb rc ])
  | Srl (ra, rb, shamt) -> (pc + 4, [ Opcode.opcode_srl ra rb shamt ])
  | Srlr (ra, rb, rc) -> (pc + 4, [ Opcode.opcode_srlr ra rb rc ])
  | Sub (ra, rb, rc) -> (pc + 4, [ Opcode.opcode_sub ra rb rc ])
  | Subi (ra, rb, imm) -> (pc + 4, [ Opcode.opcode_subi ra rb imm ])
  | Subiu (ra, rb, imm) -> (pc + 4, [ Opcode.opcode_subiu ra rb imm ])
  | Subu (ra, rb, rc) -> (pc + 4, [ Opcode.opcode_subu ra rb rc ])
  | Sw (RegOffset (r, o), ra) -> (pc + 4, [ Opcode.opcode_store ra r 2 o ])
  | Swap (ra, rb) ->
      ( pc + 12,
        [
          Opcode.opcode_xor ra ra rb;
          Opcode.opcode_xor rb ra rb;
          Opcode.opcode_xor ra ra rb;
        ] )
  | Trap -> (pc + 4, [ Opcode.opcode_trap ])
  | Xor (ra, rb, rc) -> (pc + 4, [ Opcode.opcode_xor ra rb rc ])
  | Xori (ra, rb, imm) -> (pc + 4, [ Opcode.opcode_xori ra rb imm ])
  | _ -> failwith "???"

let resolve_stmt stmt pc =
  match stmt with
  | Instruction instr -> resolve_instruction instr pc
  | Label label ->
      if Hashtbl.mem symtable label then failwith "sym already defined"
      else Hashtbl.add symtable label pc;
      (pc, [])
  | _ -> (pc, [])

let resolve_to_opcode (Program stmts) =
  let rec resolve_ast stmts pc =
    match stmts with
    | [] -> []
    | h :: t ->
        let x, i = resolve_stmt h pc in
        i @ resolve_ast t x
  in
  let pc = 0 in
  resolve_ast stmts pc

let resolve_register opr =
  match opr with
  | 0, ra, rb, rc, 0, 1 -> Add (ra, rb, rc)
  | 0, ra, rb, rc, 0, 2 -> Addu (ra, rb, rc)
  | 0, ra, rb, rc, 0, 5 -> And (ra, rb, rc)
  | 0, ra, rb, rc, 0, 6 -> Div (ra, rb, rc)
  | 0, ra, rb, rc, 0, 7 -> Divu (ra, rb, rc)
  | 0, ra, rb, rc, 0, 19 -> Mod (ra, rb, rc)
  | 0, ra, rb, rc, 0, 20 -> Modu (ra, rb, rc)
  | 0, ra, rb, rc, rd, 8 -> Mult (ra, rb, rc, rd)
  | 0, ra, rb, rc, rd, 9 -> Multu (ra, rb, rc, rd)
  | 63, ra, sr, 0, 0, 62 -> Mvsrr (ra, sr)
  | 63, sr, ra, 0, 0, 63 -> Mvsrw (sr, ra)
  | 0, ra, rb, rc, 0, 10 -> Nor (ra, rb, rc)
  | 0, ra, rb, rc, 0, 11 -> Or (ra, rb, rc)
  | 0, ra, rb, 0, shamt, 13 -> Sll (ra, rb, shamt)
  | 0, ra, rb, rc, 0, 16 -> Sllr (ra, rb, rc)
  | 0, ra, rb, 0, shamt, 15 -> Sra (ra, rb, shamt)
  | 0, ra, rb, rc, 0, 17 -> Srar (ra, rb, rc)
  | 0, ra, rb, 0, shamt, 14 -> Srl (ra, rb, shamt)
  | 0, ra, rb, rc, 0, 18 -> Srlr (ra, rb, rc)
  | 0, ra, rb, rc, 0, 3 -> Sub (ra, rb, rc)
  | 0, ra, rb, rc, 0, 4 -> Subu (ra, rb, rc)
  | 0, ra, rb, rc, 0, 12 -> Xor (ra, rb, rc)
  | _ -> failwith "Unknown Opcode"

let resolve_immediat opim =
  match opim with
  | 1, ra, rb, imm -> Addi (ra, rb, imm)
  | 2, ra, rb, imm -> Addiu (ra, rb, imm)
  | 5, ra, rb, imm -> Andi (ra, rb, imm)
  | 14, ra, rb, offset -> Beq (ra, rb, Raw offset)
  | 17, ra, rb, offset -> Bge (ra, rb, Raw offset)
  | 19, ra, rb, offset -> Bgeu (ra, rb, Raw offset)
  | 16, ra, rb, offset -> Blt (ra, rb, Raw offset)
  | 18, ra, rb, offset -> Bltu (ra, rb, Raw offset)
  | 15, ra, rb, offset -> Bne (ra, rb, Raw offset)
  | 11, ra, 0, imm -> Lih (ra, imm)
  | 6, ra, rb, imm -> Ori (ra, rb, imm)
  | 3, ra, rb, imm -> Subi (ra, rb, imm)
  | 4, ra, rb, imm -> Subiu (ra, rb, imm)
  | 7, ra, rb, imm -> Xori (ra, rb, imm)
  | _ -> failwith "Unknown opcode"

let resolve_loadstore opls =
  match opls with
  | 9, ra, rb, 0, offset -> Lb (ra, RegOffset (rb, offset))
  | 9, ra, rb, 1, offset -> Lh (ra, RegOffset (rb, offset))
  | 9, ra, rb, 2, offset -> Lw (ra, RegOffset (rb, offset))
  | 12, ra, rb, 0, offset -> Lbu (ra, RegOffset (rb, offset))
  | 12, ra, rb, 1, offset -> Lhu (ra, RegOffset (rb, offset))
  | 8, ra, rb, 0, offset -> Sb (RegOffset (rb, offset), ra)
  | 8, ra, rb, 1, offset -> Sh (RegOffset (rb, offset), ra)
  | 8, ra, rb, 2, offset -> Sw (RegOffset (rb, offset), ra)
  | _ -> failwith "Unknown opcode"

let resolve_jump opj =
  match opj with
  | 13, ra, offset -> Call (RegOffset (ra, offset))
  | 10, ra, offset -> Jmp (RegOffset (ra, offset))
  | _ -> failwith "Unknown opcode"

let resolve_to_ast ops =
  let rec resolve_opcodes ops =
    match ops with
    | [] -> []
    | h :: t -> (
        match h with
        | Opcode.Register opr ->
            [ Ast.Instruction (resolve_register opr) ] @ resolve_opcodes t
        | Opcode.Immediat opi ->
            [ Ast.Instruction (resolve_immediat opi) ] @ resolve_opcodes t
        | Opcode.LoadStore opls ->
            [ Ast.Instruction (resolve_loadstore opls) ] @ resolve_opcodes t
        | Opcode.Jump opj ->
            [ Ast.Instruction (resolve_jump opj) ] @ resolve_opcodes t)
  in
  Program (resolve_opcodes ops)
