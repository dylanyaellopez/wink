open Token
open Resolver

let source =
  let path = Array.get Sys.argv 1 in
  In_channel.with_open_text path In_channel.input_all

let _ =
  let state = new_state source in
  let resolver ast_stmts =
    let* () = Lang_items.import_foreign Lang_items.load_prims in
    stmts ast_stmts
  in

  match Ast.stmts (Parser.new_state source) with
  | Ok (stmts, _) -> (
      match resolver stmts state with
      | Ok ((str1, str2), _) -> print_endline (str1 ^ "\n" ^ str2)
      | Err s -> print_endline s)
  | Err -> print_endline "error parser"
