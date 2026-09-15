open Token
open Parser

type pat = Var of tok | Tuple of pat list
and expr = Const of tok | Bin of tok * expr * expr | App of expr * expr
and stmt = Binding of tok * pat list * expr

let rec pat s =
  (map (fun x -> Var x) (token Ident)
  || let* _ = token LParen in
     let* list = many_separated pat Equal in
     let* _ = token RParen in
     pure (Tuple list))
    s

let rec binary next ops s =
  s
  |> let* lexpr = next in
     let* opt_rexpr =
       opt
         (let* op = tokens ops in
          let* rexpr = binary next ops in
          pure (op, rexpr))
     in
     match opt_rexpr with
     | Some (op, rexpr) -> pure (Bin (op, lexpr, rexpr))
     | None -> pure lexpr

let rec expr s = term s
and term s = s |> binary factor [ Plus; Minus ]
and factor s = s |> binary call [ Star; Slash ]

and call s =
  ( let* ) primary (fold_many (fun acc expr -> App (acc, expr)) primary) s

and primary s = map (fun x -> Const x) (tokens [ Ident; Number ]) s

let stmt =
  let* _ = token Let in
  let* ident = token Ident in
  let* params = many pat in
  let* _ = token Equal in
  let* expr = expr in
  pure (Binding (ident, params, expr))

let stmts = many_to_end stmt
