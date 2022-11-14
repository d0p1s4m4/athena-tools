let preprocess file =
  let tmp_file = Filename.temp_file file "prep" in
  let _ = Sys.command ("cpp " ^ file ^ " > " ^ tmp_file) in
  tmp_file

let read_file file =
  let ch = open_in file in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let compile_file file =
  let f = if Filename.check_suffix file ".S" then preprocess file else file in
  let s = read_file f in
  let ast = Parser.program Scanner.tokenize (Lexing.from_string s) in
  let ops = Resolve.resolve_to_opcode ast in
  Dump.dump (Resolve.resolve_to_ast ops)
