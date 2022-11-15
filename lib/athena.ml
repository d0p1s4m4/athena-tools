let preprocess file =
  let tmp_file = Filename.temp_file file "prep" in
  let _ = Sys.command ("cpp " ^ file ^ " > " ^ tmp_file) in
  tmp_file

let read_file file =
  let ch = open_in file in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let write_file file opcodes =
  let obj_file = (Filename.remove_extension file) ^ ".o" in
  let ch = open_out_bin obj_file in
  let rec write_bin ch opcodes = 
    match opcodes with
    | [] -> ()
    | h :: t -> (output_bytes ch h; write_bin ch t)
  in write_bin ch opcodes; flush ch; close_out ch

let compile_file file =
  let f = if Filename.check_suffix file ".S" then preprocess file else file in
  let s = read_file f in
  let ast = Parser.program Scanner.tokenize (Lexing.from_string s) in
  let ops = Resolve.resolve_to_opcode ast in
  let enc = (List.map Encode.encode_opcode ops) in
  write_file file enc; 
  Dump.dump (Resolve.resolve_to_ast ops)
