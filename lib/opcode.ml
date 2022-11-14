(*             opcode * ra * rb * imm *)
type iformat = int * int * int * int

(*             opcode * ra * rb * rc * shamt * func *)
type rformat = int * int * int * int * int * int

(*             opcode * ra * rb * size * offset *)
type lsformat = int * int * int * int * int

(*             opcode * ra * offset *)
type jformat = int * int * int

type opcode =
  | Immediat of iformat
  | Register of rformat
  | Jump of jformat
  | LoadStore of lsformat

type binary = Opcodes of opcode list

let opcode_add ra rb rc = Register (0, ra, rb, rc, 0, 1)
let opcode_addi ra rb imm = Immediat (1, ra, rb, imm)
let opcode_addiu ra rb imm = Immediat (2, ra, rb, imm)
let opcode_addu ra rb rc = Register (0, ra, rb, rc, 0, 2)
let opcode_and ra rb rc = Register (0, ra, rb, rc, 0, 5)
let opcode_andi ra rb imm = Immediat (5, ra, rb, imm)
let opcode_beq ra rb offset = Immediat (14, ra, rb, offset)
let opcode_bge ra rb offset = Immediat (17, ra, rb, offset)
let opcode_bgeu ra rb offset = Immediat (19, ra, rb, offset)
let opcode_blt ra rb offset = Immediat (16, ra, rb, offset)
let opcode_bltu ra rb offset = Immediat (18, ra, rb, offset)
let opcode_bne ra rb offset = Immediat (15, ra, rb, offset)
let opcode_call ra offset = Jump (13, ra, offset)
let opcode_div ra rb rc = Register (0, ra, rb, rc, 0, 6)
let opcode_divu ra rb rc = Register (0, ra, rb, rc, 0, 7)
let opcode_jmp ra offset = Jump (10, ra, offset)
let opcode_load ra rb size offset = LoadStore (9, ra, rb, size, offset)
let opcode_loadu ra rb size offset = LoadStore (12, ra, rb, size, offset)
let opcode_lih ra imm = Immediat (11, ra, 0, imm)
let opcode_mod ra rb rc = Register (0, ra, rb, rc, 0, 19)
let opcode_modu ra rb rc = Register (0, ra, rb, rc, 0, 20)
let opcode_mult ra rb rc rd = Register (0, ra, rb, rc, rd, 8)
let opcode_multu ra rb rc rd = Register (0, ra, rb, rc, rd, 9)
let opcode_mvsrr ra sr = Register (63, ra, sr, 0, 0, 62)
let opcode_mvsrw sr ra = Register (63, sr, ra, 0, 0, 63)
let opcode_nor ra rb rc = Register (0, ra, rb, rc, 0, 10)
let opcode_or ra rb rc = Register (0, ra, rb, rc, 0, 11)
let opcode_ori ra rb imm = Immediat (6, ra, rb, imm)
let opcode_rett = Register (0, 0, 0, 0, 0, 62)
let opcode_store ra rb size offset = LoadStore (8, ra, rb, size, offset)
let opcode_sll ra rb shamt = Register (0, ra, rb, 0, shamt, 13)
let opcode_sllr ra rb rc = Register (0, ra, rb, rc, 0, 16)
let opcode_sra ra rb shamt = Register (0, ra, rb, 0, shamt, 15)
let opcode_srar ra rb rc = Register (0, ra, rb, rc, 0, 17)
let opcode_srl ra rb shamt = Register (0, ra, rb, 0, shamt, 14)
let opcode_srlr ra rb rc = Register (0, ra, rb, rc, 0, 18)
let opcode_sub ra rb rc = Register (0, ra, rb, rc, 0, 3)
let opcode_subi ra rb imm = Immediat (3, ra, rb, imm)
let opcode_subiu ra rb imm = Immediat (4, ra, rb, imm)
let opcode_subu ra rb rc = Register (0, ra, rb, rc, 0, 4)
let opcode_trap = Register (0, 0, 0, 0, 0, 63)
let opcode_xor ra rb rc = Register (0, ra, rb, rc, 0, 12)
let opcode_xori ra rb imm = Immediat (7, ra, rb, imm)
