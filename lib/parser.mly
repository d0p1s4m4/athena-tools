%{
	open Ast
%}

%token <int> INT
%token <string> STRING IDENT
%token ORG INCBIN SECTION ALIGN EXTERN GLOBAL INCLUDE
%token ADD ADDI ADDIU ADDU AND ANDI BEQ BEQZ BGT BGTZ
%token BLT BLTZ BNE BNEZ CALL DIV DIVU JMP LIH MOD MODU
%token MULT MULTU NOP NOR OR ORI SLL SLLR SRA SRAR SRL SRLR
%token SUB SUBI SUBIU SUBU XOR XORI TRAP LB LBU SB LH LHU
%token SH LW SW MVSRR MVSRW B LA
%token R0 R1 R2 R3 R4 R5 R6 R7 R8 R9 R10 R11 R12 R13 R14 R15
%token R16 R17 R18 R19 R20 R21 R22 R23 R24 R25 R26 R27 R28 R29
%token R30 R31
%token SR_ISA SR_VENDORID SR_STATUS SR_TRAPVEC SR_INTEN SR_EPC
%token SR_CAUSE
%token COMMA LPAREN RPAREN COLON EOF

%start <Ast.program> program

%%

program: stmts EOF { Program $1 }
	;

stmts: stmt { [$1] } 
	| stmts stmt { $1@[$2] }
	;

stmt: instruction { Instruction($1) }
	| directive { Directive($1) }
	| IDENT COLON { Label($1) }
	;

instruction: ADD register COMMA register COMMA register { Add($2, $4, $6) }
	| ADDI register COMMA register COMMA immediat { Addi($2, $4, $6) }
	| ADDIU register COMMA register COMMA immediat { Addiu($2, $4, $6) }
	| ADDU register COMMA register COMMA register { Addu($2, $4, $6) }
	| AND register COMMA register COMMA register { And($2, $4, $6) }
	| ANDI register COMMA register COMMA immediat { Andi($2, $4, $6) }
	| B relative_address { Beq(0, 0, $2) }
	| BEQ register COMMA register COMMA relative_address { Beq($2, $4, $6) }
	| BEQZ register COMMA relative_address { Beq($2, 0, $4) }
	| BGT register COMMA register COMMA relative_address { Bgt($2, $4, $6) }
	| BGTZ register COMMA relative_address { Bgt($2, 0, $4) }
	| BLT register COMMA register COMMA relative_address { Blt($2, $4, $6) }
	| BLTZ register COMMA relative_address { Blt($2, 0, $4) }
	| BNE register COMMA register COMMA relative_address { Bne($2, $4, $6) }
	| BNEZ register COMMA relative_address { Bne($2, 0, $4) }
	| CALL full_address { Call($2) }
	| DIV register COMMA register COMMA register { Div($2, $4, $6) }
	| DIVU register COMMA register COMMA register { Div($2, $4, $6) }
	| JMP full_address { Jmp($2) }
	| LA register COMMA relative_address { La($2, $4) }
	| LIH register COMMA immediat { Lih($2, $4) }
	| MOD register COMMA register COMMA register { Mod($2, $4, $6) }
	| MODU register COMMA register COMMA register { Modu($2, $4, $6) }
	| MULT register COMMA register COMMA register COMMA register { Mult($2, $4, $6, $8) }
	| MULTU register COMMA register COMMA register COMMA register { Multu($2, $4, $6, $8) }
	| NOP { Nop }
	| NOR register COMMA register COMMA register { Nor($2, $4, $6) }
	| OR register COMMA register COMMA register { Or($2, $4, $6) }
	| ORI register COMMA register COMMA immediat { Ori($2, $4, $6) }
	| SLL register COMMA register COMMA shamt { Sll($2, $4, $6) }
	| SLLR register COMMA register COMMA register { Sllr($2, $4, $6) }
	| SRA register COMMA register COMMA shamt { Sra($2, $4, $6) }
	| SRAR register COMMA register COMMA register { Srar($2, $4, $6) }
	| SRL register COMMA register COMMA shamt { Srl($2, $4, $6) }
	| SRLR register COMMA register COMMA register { Srlr($2, $4, $6) }
	| SUB register COMMA register COMMA register { Sub($2, $4, $6) }
	| SUBI register COMMA register COMMA immediat { Subi($2, $4, $6) }
	| SUBIU register COMMA register COMMA immediat { Subiu($2, $4, $6) }
	| SUBU register COMMA register COMMA register { Subu($2, $4, $6) }
	| TRAP { Trap }
	| XOR register COMMA register COMMA register { Xor($2, $4, $6) }
	| XORI register COMMA register COMMA immediat { Xori($2, $4, $6) }
	| LB register COMMA full_address { Lb($2, $4) }
	| LBU register COMMA full_address { Lbu($2, $4) }
	| LH register COMMA full_address { Lh($2, $4) }
	| LHU register COMMA full_address { Lhu($2, $4) }
	| LW register COMMA full_address { Lw($2, $4) }
	| SB full_address COMMA register { Sb($2, $4) }
	| SH full_address COMMA register { Sh($2, $4) }
	| SW full_address COMMA register { Sw($2, $4) }
	| MVSRR register COMMA special_register { Mvsrr($2, $4) }
	| MVSRW special_register COMMA register { Mvsrw($2, $4) }
	;

relative_address: INT { Raw($1) }
	| IDENT { RLabel($1) }
	;

full_address: offset LPAREN register RPAREN { RegOffset($3, $1) }
	| IDENT { FLabel($1) }
	| register { RegOffset($1, 0) }
	;

directive: ORG INT { Org($2) }
	| INCBIN STRING { Incbin($2) }
	| SECTION IDENT { Section($2) }
	| ALIGN INT { Align($2) }
	| EXTERN IDENT { Extern($2) }
	| GLOBAL IDENT { Global($2) }
	| INCLUDE STRING { Include($2) }
	;

immediat: INT { $1 }
	;

shamt: INT { $1 }
	;

offset: INT { $1 }
	;

special_register: SR_ISA { 0 }
	| SR_VENDORID { 1 }
	| SR_STATUS { 2 }
	| SR_TRAPVEC { 3 }
	| SR_INTEN { 4 }
	| SR_EPC { 5 }
	| SR_CAUSE { 6 }
	;

register: R0 { 0 }
	| R1 { 1 }
	| R2 { 2 }
    | R3 { 3 }
    | R4 { 4 }
    | R5 { 5 }
    | R6 { 6 }
    | R7 { 7 }
    | R8 { 8 }
    | R9 { 9 }
    | R10 { 10 }
    | R11 { 11 }
    | R12 { 12 }
    | R13 { 13 }
    | R14 { 14 }
    | R15 { 15 }
    | R16 { 16 }
    | R17 { 17 }
    | R18 { 18 }
    | R19 { 19 }
    | R20 { 20 }
    | R21 { 21 }
    | R22 { 22 }
    | R23 { 23 }
    | R24 { 24 }
    | R25 { 25 }
    | R26 { 26 }
    | R27 { 27 }
    | R28 { 28 }
    | R29 { 29 }
    | R30 { 30 }
    | R31 { 31 }
	;

%%
