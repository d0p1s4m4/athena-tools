let encode_opcode opcode =
  let instr = match opcode with
  | Opcode.Register(op, ra, rb, rc, shamt, func) -> 
    let instr = op lor (ra lsl 6) lor (rb lsl 11) lor (rc lsl 16) lor (shamt lsl 21) lor (func lsl 26) in
    Int32.of_int instr
  | Opcode.Immediat(op, ra, rb, imm) ->
    let instr = op lor (ra lsl 6) lor (rb lsl 11) lor (imm lsl 16) in
    Int32.of_int instr
  | Opcode.LoadStore(op, ra, rb, size, offset) ->
    let instr = op lor (ra lsl 6) lor (rb lsl 11) lor (size lsl 16) lor (offset lsl 18) in
    Int32.of_int instr
  | Opcode.Jump(op, ra, offset) -> let instr = op lor (ra lsl 6) lor (offset lsl 11) in 
    Int32.of_int instr
  in
  let b = Bytes.create 4 in
  Bytes.set_int32_be b 0 instr; b
