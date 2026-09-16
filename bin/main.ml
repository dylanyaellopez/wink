open Parser
open Token

let source =
  let path = Array.get Sys.argv 1 in
  In_channel.with_open_text path In_channel.input_all

let _ =
  match Ast.stmts (new_state source) with
  | Ok (stmts, _) -> (
      match Resolver.stmts stmts (Resolver.new_state source) with
      | Ok ((str1, str2), _) -> print_endline (str1 ^ "\n" ^ str2)
      | Err s -> print_endline s)
  | Err -> print_endline "error parser"
