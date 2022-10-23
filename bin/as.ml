open Cmdliner

let version = "%%VERSION%%"

let athena_as files =
  print_endline (String.concat ", " (List.map Athena.compile_file files))

let files = Arg.(non_empty & pos_all file [] & info [] ~docv:"FILE")

let cmd =
  let doc = "Athena Assembler" in
  let sdocs = Manpage.s_common_options in
  let info = Cmd.info "as" ~version ~doc ~sdocs in
  Cmd.v info Term.(const athena_as $ files)

let () = exit (Cmd.eval cmd)
