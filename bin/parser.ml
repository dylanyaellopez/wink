open Token

type parser_state = { source : string; pos : int }
type 'a parser_result = Ok of 'a * parser_state | Err

let ( let* ) p f s = match p s with Ok (res, s) -> f res s | Err -> Err
let map f p s = match p s with Ok (res, s) -> Ok (f res, s) | Err -> Err

let token ty s =
  match Lexer.lex_token s.source s.pos with
  | Some ({ ty = ty' } as tok) when ty' = ty ->
      Ok (tok, { s with pos = tok.pos + tok.len })
  | Some tok -> Err
  | None -> Err

let opt p s =
  match p s with Ok (res, s) -> Ok (Some res, s) | Err -> Ok (None, s)

let ( || ) p1 p2 s = match p1 s with Ok (res, s) -> Ok (res, s) | Err -> p2 s

let rec many' p acc s =
  match p s with Ok (res, s) -> many' p (res :: acc) s | Err -> Ok (acc, s)

let many p = map List.rev (many' p [])
let many_rev p = many' p []

let rec many_separated' p sep acc s =
  match p s with
  | Ok (res, s) -> (
      match token sep s with
      | Ok (_, s) -> many_separated' p sep (res :: acc) s
      | Err -> Ok (res :: acc, s))
  | Err -> Err

let many_separated p sep = map List.rev (many_separated' p sep [])
let many_separated_rev p sep = many_separated' p sep []

let rec fold_many f p acc s =
  match p s with
  | Ok (res, s) -> fold_many f p (f acc res) s
  | Err -> Ok (acc, s)
