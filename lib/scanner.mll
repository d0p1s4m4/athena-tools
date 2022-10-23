{
	open Parser

    exception SyntaxError of string

    let keyword_tbl = Hashtbl.create 5
    let directive_tbl = Hashtbl.create 5
    let register_tbl = Hashtbl.create 48

    let _ = List.iter (fun (name, instruction) ->
            Hashtbl.add keyword_tbl name instruction) [
                "add", ADD;
                "addi", ADDI;
                "addiu", ADDIU;
                "addu", ADDU;
                "and", AND;
                "andi", ANDI;
                "b", B;
                "beq", BEQ;
                "beqz", BEQZ;
                "bgt", BGT;
                "bgtz", BGTZ;
                "blt", BLT;
                "bltz", BLTZ;
                "bne", BNE;
                "bnez", BNEZ;
                "call", CALL;
                "div", DIV;
                "divu", DIVU;
                "jmp", JMP;
                "lb", LB;
                "lbu", LBU;
                "lh", LH;
                "lhu", LHU;
                "lih", LIH;
                "lw", LW;
                "mod", MOD;
                "modu", MODU;
                "mult", MULT;
                "multu", MULTU;
                "mvsrr", MVSRR;
                "mvsrw", MVSRW;
                "nop", NOP;
                "nor", NOR;
                "or", OR;
                "ori", ORI;
                "sb", SB;
                "sh", SH;
                "sll", SLL;
                "sllr", SLLR;
                "sra", SRA;
                "srar", SRAR;
                "srl", SRL;
                "srlr", SRLR;
                "sub", SUB;
                "subi", SUBI;
                "subiu", SUBIU;
                "subu", SUBU;
                "sw", SW;
                "trap", TRAP;
                "xor", XOR;
                "xori", XORI;
        ];
        List.iter (fun (name, directive) ->
            Hashtbl.add directive_tbl name directive) [
                ".org", ORG;
                ".incbin", INCBIN;
                ".section", SECTION;
                ".include", INCLUDE;
                ".extern", EXTERN;
                ".global", GLOBAL;
        ];
        List.iter (fun (name, register) ->
            Hashtbl.add register_tbl name register) [
                "r0", R0;
                "zero", R0;
                "r1", R1;
                "v0", R1;
                "r2", R2;
                "v1", R2;
                "r3", R3;
                "v2", R3;
                "r4", R4;
                "v3", R4;
                "r5", R5;
                "v4", R5;
                "r6", R6;
                "v5", R6;
                "r7", R7;
                "v6", R7;
                "r8", R8;
                "v7", R8;
                "r9", R9;
                "a0", R9;
                "r10", R10;
                "a1", R10;
                "r11", R11;
                "r12", R12;
                "r13", R13;
                "r14", R14;
                "r15", R15;
                "r16", R16;
                "r17", R17;
                "r18", R18;
                "r19", R19;
                "r20", R20;
                "r21", R21;
                "r22", R22;
                "r23", R23;
                "r24", R24;
                "r25", R25;
                "k0", R25;
                "r26", R26;
                "k1", R26;
                "r27", R27;
                "at", R27;
                "r28", R28;
                "sp", R28;
                "r29", R29;
                "gp", R29;
                "r30", R30;
                "fp", R30;
                "r31", R31;
                "ra", R31;

                (* special registers *)
                "isa", SR_ISA ;
                "vendorid", SR_VENDORID;
                "status", SR_STATUS;
                "trapvec", SR_TRAPVEC;
                "inten", SR_INTEN;
                "epc", SR_EPC;
                "cause", SR_CAUSE;
        ]
}

let digit = ['0' - '9']
let alpha = ['a' - 'z' 'A' - 'Z']
let sign = ['-' '+']
let hexvalue = "0x" ['A' - 'F' 'a' - 'f' '0' - '9']

let integer = sign? digit+

let identifier = (alpha | '_' | '.') (alpha | digit | '_')*
let whitespace = [' ' '\n' '\r' '\t']+

rule tokenize = parse
    | '#' { comment lexbuf } (* ignore c preprocessor *)
    | ";" { comment lexbuf }
    | '"' { str (Buffer.create 1024) lexbuf }
    | ',' { COMMA }
    | '(' { LPAREN }
    | ')' { RPAREN }
    | ':' { COLON }
    | identifier as instr {
            try Hashtbl.find keyword_tbl instr
            with Not_found ->
                try Hashtbl.find register_tbl instr
                with Not_found -> 
                    try Hashtbl.find directive_tbl instr 
                    with Not_found -> IDENT instr
        }
    | hexvalue { INT (int_of_string (Lexing.lexeme lexbuf ))}
	| integer { INT (int_of_string (Lexing.lexeme lexbuf))}
	| whitespace { tokenize lexbuf }
	| eof { EOF }
	| _ { raise (SyntaxError ("???: '" ^ Lexing.lexeme lexbuf ^ "'")) }

and comment = parse
    | '\n' { tokenize lexbuf }
    | _ { comment lexbuf }

and str buff = parse
    | '"' { STRING(Buffer.contents buff) }
    | '\\' '\\' { Buffer.add_char buff '\\'; str buff lexbuf }
    | _ as c { Buffer.add_char buff c; str buff lexbuf }
    | eof { raise (SyntaxError "Unexpected EOF") }
