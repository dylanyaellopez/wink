open Token

let rec skip_whitespace source pos =
  if String.length source <= pos then pos
  else
    match source.[pos] with
    | '\n' | ' ' | '\t' | '\r' -> skip_whitespace source (pos + 1)
    | _ -> pos

let is_alpha c =
  match c with 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false

let is_numeric c = match c with '0' .. '9' -> true | _ -> false
let is_alpha_numeric c = is_alpha c || is_numeric c

let is_symbol c =
  match c with
  | '!' | '@' | '#' | '$' | '%' | '^' | '&' | '*' | '-' | '+' | '=' | ':' | '/'
  | '|' | '.' | ',' | '<' | '>' | '?' ->
      true
  | _ -> false

let rec ident source pos =
  if String.length source <= pos then pos
  else if is_alpha_numeric source.[pos] then ident source (pos + 1)
  else pos

let rec number source pos =
  if String.length source <= pos then pos
  else if is_numeric source.[pos] then number source (pos + 1)
  else pos

let rec symbol source pos =
  if String.length source <= pos then pos
  else if is_symbol source.[pos] then symbol source (pos + 1)
  else pos

let lex_token source pos =
  let pos = skip_whitespace source pos in
  let tok ty len = Some { ty; pos; len } in
  let peek x =
    if String.length source <= pos + x then '\000' else source.[pos + x]
  in
  if String.length source <= pos then tok EOF 0
  else
    match source.[pos] with
    | '(' -> tok LParen 1
    | ')' -> tok RParen 1
    | '#' when peek 1 == '!' -> tok Attr 2
    | '=' -> tok Equal 1
    | 'a' .. 'z' | '_' -> (
        let len = ident source pos - pos in
        match String.sub source pos len with
        | "let" -> tok Let len
        | "in" -> tok In len
        | _ -> tok Ident len)
    | 'A' .. 'Z' -> tok UIdent (ident source (pos + 1) - pos)
    | '0' .. '9' -> tok Number (number source (pos + 1) - pos)
    | s when is_symbol s -> tok OpIdent (symbol source (pos + 1) - pos)
    | _ -> None
