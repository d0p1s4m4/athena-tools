type relative_address = Raw of int | RLabel of string
type full_address = RegOffset of int * int | FLabel of string

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
  | La of int * relative_address (* pseudo instr *)
  | Lb of int * full_address
  | Lbu of int * full_address 
  | Lh of int * full_address 
  | Lhu of int * full_address 
  | Lih of int * int
  | Lw of int * full_address
  | Mod of int * int * int
  | Modu of int * int * int
  | Mult of int * int * int * int
  | Multu of int * int * int * int
  | Mvsrr of int * int
  | Mvsrw of int * int
  | Nop
  | Nor of int * int * int
  | Or of int * int * int
  | Ori of int * int * int
  | Sb of full_address * int
  | Sh of full_address * int
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
  | Sw of full_address * int
  | Trap
  | Xor of int * int * int
  | Xori of int * int * int

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

type stmt =
  | Instruction of instruction
  | Directive of directive
  | Label of string

type program = Program of stmt list
