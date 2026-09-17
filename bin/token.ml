type tok_ty =
  | UIdent
  | Ident
  | OpIdent
  | LParen
  | RParen
  | Attr
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
  | OpIdent -> "OpIdent"
  | LParen -> "("
  | RParen -> ")"
  | Attr -> "#!"
  | Number -> "Number"
  | Let -> "let"
  | Equal -> "="
  | In -> "in"
  | EOF -> "EOF"

let show_tok tok source = String.sub source tok.pos tok.len
