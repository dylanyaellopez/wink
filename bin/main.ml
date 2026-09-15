open Lexer

let () =
  let path = Array.get Sys.argv 1 in
  let source = In_channel.with_open_text path In_channel.input_all in
  let _tokens = lex_token source 0 in
  ()
