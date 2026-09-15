open Parser
open Token

let source =
  let path = Array.get Sys.argv 1 in
  In_channel.with_open_text path In_channel.input_all

let _ =
  match Ast.stmts (new_state source) with
  | Ok (stmts, _) -> print_endline (Int.to_string (List.length stmts))
  | Err -> print_endline "error"
