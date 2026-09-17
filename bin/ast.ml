open Token
open Parser

type attribute = tok * tok option

type pat = Decl of tok | Tuple of pat list

and expr =
  | Const of tok
  | Var of tok
  | Bin of tok * expr * expr
  | App of expr * expr

and stmt = Binding of attribute option * tok * pat list * expr

type prec =
  | PAny
  | PFactor
  | PTerm
  | PColon
  | PEq
  | PComp
  | PAmp
  | PCaret
  | PPipe

let op prec s =
  match token OpIdent s with
  | Ok (tok, s) -> (
      let c = (String.sub s.source tok.pos tok.len).[0] in
      match (c, prec) with
      | ('*' | '/' | '%'), PFactor -> Ok (tok, s)
      | ('+' | '-'), PTerm -> Ok (tok, s)
      | ':', PColon -> Ok (tok, s)
      | ('=' | '!'), PEq -> Ok (tok, s)
      | ('<' | '>'), PComp -> Ok (tok, s)
      | '&', PAmp -> Ok (tok, s)
      | '^', PCaret -> Ok (tok, s)
      | '|', PPipe -> Ok (tok, s)
      | _, PAny -> Ok (tok, s)
      | _ -> Err)
  | Err -> Err

let rec pat s =
  (map (fun x -> Decl x) (token Ident)
  || let* _ = token LParen in
     let* list = many_separated pat Equal in
     let* _ = token RParen in
     pure (Tuple list))
    s

let rec binary next prec s =
  s
  |> let* lexpr = next in
     let* opt_rexpr =
       opt
         (let* op = op prec in
          let* rexpr = binary next prec in
          pure (op, rexpr))
     in
     match opt_rexpr with
     | Some (op, rexpr) -> pure (Bin (op, lexpr, rexpr))
     | None -> pure lexpr

let rec expr s = term s
and term s = s |> binary factor PTerm
and factor s = s |> binary colon PFactor
and colon s = s |> binary ceq PColon
and ceq s = s |> binary comp PEq
and comp s = s |> binary amp PComp
and amp s = s |> binary caret PAmp
and caret s = s |> binary pipe PCaret
and pipe s = s |> binary call PPipe

and call s =
  ( let* ) primary (fold_many (fun acc expr -> App (acc, expr)) primary) s

and primary s =
  (map (fun x -> Const x) (token Number)
  || map
       (fun x -> Var x)
       (tokens [ Ident; UIdent ]
       || let* _ = token LParen in
          let* ident = token OpIdent in
          let* _ = token RParen in
          pure ident))
    s

let attribute =
  let* _ = token Attr in
  let* ident = token Ident in
  let* arg = opt (token Ident) in
  pure (ident, arg)

let stmt =
  let* atr = opt attribute in
  let* _ = token Let in
  let* ident =
    (let* _ = token LParen in
     let* op = token OpIdent in
     let* _ = token RParen in
     pure op)
    || token Ident
  in
  let* params = many pat in
  let* _ = token Equal in
  let* expr = expr in
  pure (Binding (atr, ident, params, expr))

let stmts = many_to_end stmt
