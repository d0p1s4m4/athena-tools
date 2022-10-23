type relative_address =
  | Raw of int
  | RelativeLabel of string

type full_address =
  | RegOffset of int * int
  | Label of string

type instruction = 
  | Add of int * int * int
  | Addi of int * int * int
  | Addiu of int * int * int
  | Addu of int * int * int
  | And of int * int * int
  | Andi of int * int * int
  | Beq of int * int * relative_address
  | Bgt of int * int * relative_address
  | Blt of int * int * relative_address
  | Bne of int * int * relative_address
  | Call of full_address
  | Div of int * int * int
  | Divu of int * int * int
  | Jmp of full_address
  | Lih of int * int
  | Mod of int * int * int
  | Modu of int * int * int
  | Mult of int * int * int * int
  | Multu of int * int * int * int
  | Nop
  | Nor of int * int * int
  | Or of int * int * int
  | Ori of int * int * int
  | Sll of int * int * int
  | Sllr of int * int * int
  | Sra of int * int * int
  | Srar of int * int * int
  | Srl of int * int * int
  | Srlr of int * int * int
  | Sub of int * int * int
  | Subi of int * int * int
  | Subiu of int * int * int
  | Subu of int * int * int
  | Trap
  | Xor of int * int * int
  | Xori of int * int * int
  | Lb of int * int * int
  | Lbu of int * int * int
  | Sb of int * int * int
  | Sbu of int * int * int
  | Lh of int * int * int
  | Lhu of int * int * int
  | Sh of int * int * int
  | Shu of int * int * int
  | Lw of int * int * int
  | Sw of int * int * int
  | Mvsrr of int * int
  | Mvsrw of int * int

type directive =
  | Org of int
  | Incbin of string
  | Align of int
  | Ascii of string
  | Asciiz of string
  | String of string list
  | Byte of int list
  | Hword of int list
  | Word of int list
  | Quad of int list
  | Extern of string
  | Global of string
  | Include of string
  | Section of string

type stmt = Instruction of instruction
   | Directive of directive
   | Label of string

type program = Program of stmt list

let dump_directive directive =
  match directive with
  | Org addr -> ".org " ^ string_of_int addr
  | Incbin file -> ".incbin " ^ file
  | Align align -> ".align " ^ string_of_int align
  | _ -> "(todo)"
  
let dump_reg reg = 
  "r" ^ string_of_int reg

let dump_instruction instruction =
  match instruction with
  | Ori (ra, rb, imm) -> "ori " ^ (dump_reg ra) ^ " " ^ (dump_reg rb) ^ " " ^ string_of_int imm ^ ")"
  | _ -> "(todo)"

let dump_stmt stmt =  
  match stmt with
  | Directive directive -> dump_directive directive
  | Instruction instruction -> dump_instruction instruction
  | Label label -> label ^ ":"

let dump (Program stmts) = String.concat "\n" (List.map dump_stmt stmts)
