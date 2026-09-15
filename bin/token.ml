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


type tok = {
  ty : tok_ty; 
  pos : int;
  len : int;
}
