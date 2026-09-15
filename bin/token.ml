type tok_ty =
  | UIdent
  | Ident
  | LParen
  | RParen
  | Plus
  | Minus
  | Star
  | Slash
  | Number
  | Let
  | Equal
  | In
  | EOF

type tok = { ty : tok_ty; pos : int; len : int }

let show_tok_ty ty =
  match ty with
  | UIdent -> "UIdent"
  | Ident -> "Ident"
  | LParen -> "("
  | RParen -> ")"
  | Plus -> "+"
  | Minus -> "-"
  | Star -> "*"
  | Slash -> "/"
  | Number -> "Number"
  | Let -> "let"
  | Equal -> "="
  | In -> "in"
  | EOF -> "EOF"

let show_tok tok source =
  match tok.ty with
  | UIdent | Ident | Number -> String.sub source tok.pos tok.len
  | _ -> show_tok_ty tok.ty
