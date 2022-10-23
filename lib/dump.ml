open Ast

let dump_directive directive =
  match directive with
  | Org addr -> ".org " ^ string_of_int addr
  | Incbin file -> ".incbin " ^ file
  | Align align -> ".align " ^ string_of_int align
  | Ascii str -> ".ascii \"" ^ str ^ "\""
  | Asciiz str -> ".asciiz\"" ^ str ^ "\\0\""
  | _ -> "(todo)"

let dump_reg reg = "r" ^ string_of_int reg

let dump_spe_reg sr =
  let sr_str =
    [ "isa"; "vendorid"; "status"; "trapvec"; "inten"; "epc"; "cause" ]
  in
  List.nth sr_str sr

let dump_r_format ra rb rc =
  dump_reg ra ^ ", " ^ dump_reg rb ^ ", " ^ dump_reg rc

let dump_i_format ra rb imm =
  dump_reg ra ^ ", " ^ dump_reg rb ^ ", " ^ string_of_int imm

let dump_rel_addr addr =
  match addr with Raw i -> string_of_int i | RLabel l -> l

let dump_abs_addr addr =
  match addr with
  | RegOffset (0, off) -> string_of_int off
  | RegOffset (ra, off) -> string_of_int off ^ "(" ^ dump_reg ra ^ ")"
  | FLabel l -> l

let dump_instruction instruction =
  match instruction with
  | Add (ra, rb, rc) -> "add " ^ dump_r_format ra rb rc
  | Addi (ra, rb, imm) -> "addi " ^ dump_i_format ra rb imm
  | Addiu (ra, rb, imm) -> "addiu " ^ dump_i_format ra rb imm
  | Addu (ra, rb, rc) -> "addu " ^ dump_r_format ra rb rc
  | And (ra, rb, rc) -> "and " ^ dump_r_format ra rb rc
  | Andi (ra, rb, imm) -> "andi " ^ dump_i_format ra rb imm
  | Beq (0, 0, addr) -> "b " ^ dump_rel_addr addr
  | Beq (ra, 0, addr) -> "beqz " ^ dump_reg ra ^ dump_rel_addr addr
  | Beq (ra, rb, addr) ->
      "beq " ^ dump_reg ra ^ ", " ^ dump_reg rb ^ ", " ^ dump_rel_addr addr
  | Bgt (ra, 0, addr) -> "bgtz " ^ dump_reg ra ^ dump_rel_addr addr
  | Bgt (ra, rb, addr) ->
      "bgt " ^ dump_reg ra ^ ", " ^ dump_reg rb ^ ", " ^ dump_rel_addr addr
  | Blt (ra, 0, addr) -> "bltz " ^ dump_reg ra ^ dump_rel_addr addr
  | Blt (ra, rb, addr) ->
      "blt " ^ dump_reg ra ^ ", " ^ dump_reg rb ^ ", " ^ dump_rel_addr addr
  | Bne (ra, 0, addr) -> "bnez " ^ dump_reg ra ^ dump_rel_addr addr
  | Bne (ra, rb, addr) ->
      "bne " ^ dump_reg ra ^ ", " ^ dump_reg rb ^ ", " ^ dump_rel_addr addr
  | Call addr -> "call " ^ dump_abs_addr addr
  | Div (ra, rb, rc) -> "div " ^ dump_r_format ra rb rc
  | Divu (ra, rb, rc) -> "divu " ^ dump_r_format ra rb rc
  | Jmp addr -> "jmp " ^ dump_abs_addr addr
  | La (ra, addr) -> "la " ^ dump_reg ra ^ ", " ^ dump_rel_addr addr
  | Lb (ra, addr) -> "lb " ^ dump_reg ra ^ ", " ^ dump_abs_addr addr
  | Lbu (ra, addr) -> "lbu " ^ dump_reg ra ^ ", " ^ dump_abs_addr addr
  | Lh (ra, addr) -> "lh " ^ dump_reg ra ^ ", " ^ dump_abs_addr addr
  | Lhu (ra, addr) -> "lhu " ^ dump_reg ra ^ ", " ^ dump_abs_addr addr
  | Li (ra, value) -> "li " ^ dump_reg ra ^ ", " ^  string_of_int value
  | Lih (ra, imm) -> "lih " ^ dump_reg ra ^ ", " ^ string_of_int imm
  | Lw (ra, addr) -> "lw " ^ dump_reg ra ^ ", " ^ dump_abs_addr addr
  | Mod (ra, rb, rc) -> "mod " ^ dump_r_format ra rb rc
  | Modu (ra, rb, rc) -> "modu " ^ dump_r_format ra rb rc
  | Mult (ra, rb, rc, rd) ->
      "mult " ^ dump_reg ra ^ ", " ^ dump_reg rb ^ ", " ^ dump_reg rc ^ ", "
      ^ dump_reg rd
  | Multu (ra, rb, rc, rd) ->
      "multu " ^ dump_reg ra ^ ", " ^ dump_reg rb ^ ", " ^ dump_reg rc ^ ", "
      ^ dump_reg rd
  | Mvsrr (ra, sr) -> "mvsrr " ^ dump_reg ra ^ ", " ^ dump_spe_reg sr
  | Mvsrw (sr, ra) -> "mvsrw " ^ dump_spe_reg sr ^ ", " ^ dump_reg ra
  | Nop -> "nop"
  | Nor (ra, rb, rc) -> "nor " ^ dump_r_format ra rb rc
  | Or (ra, rb, rc) -> "or " ^ dump_r_format ra rb rc
  | Ori (ra, rb, imm) -> "ori " ^ dump_i_format ra rb imm
  | Sb (addr, ra) -> "sb " ^ dump_abs_addr addr ^ ", " ^ dump_reg ra
  | Sh (addr, ra) -> "sh " ^ dump_abs_addr addr ^ ", " ^ dump_reg ra
  | Sll (ra, rb, shamt) -> "sll " ^ dump_i_format ra rb shamt
  | Sllr (ra, rb, rc) -> "sllr " ^ dump_r_format ra rb rc
  | Sra (ra, rb, shamt) -> "sra " ^ dump_i_format ra rb shamt
  | Srar (ra, rb, rc) -> "srar " ^ dump_r_format ra rb rc
  | Srl (ra, rb, shamt) -> "srl " ^ dump_i_format ra rb shamt
  | Srlr (ra, rb, rc) -> "srlr " ^ dump_r_format ra rb rc
  | Sub (ra, rb, rc) -> "sub " ^ dump_r_format ra rb rc
  | Subi (ra, rb, imm) -> "subi " ^ dump_i_format ra rb imm
  | Subiu (ra, rb, imm) -> "subiu " ^ dump_i_format ra rb imm
  | Subu (ra, rb, rc) -> "subu " ^ dump_r_format ra rb rc
  | Sw (addr, ra) -> "sw " ^ dump_abs_addr addr ^ ", " ^ dump_reg ra
  | Trap -> "trap"
  | Xor (ra, rb, rc) -> "xor " ^ dump_r_format ra rb rc
  | Xori (ra, rb, imm) -> "xori " ^ dump_i_format ra rb imm

let dump_stmt stmt =
  match stmt with
  | Directive directive -> dump_directive directive
  | Instruction instruction -> dump_instruction instruction
  | Label label -> label ^ ":"

let dump (Program stmts) = String.concat "\n" (List.map dump_stmt stmts)
