open Token
module StringMap = Map.Make (String)
module IntSet = Set.Make (Int)

type mod_scope = int StringMap.t

type resolver_state = {
  source : string;
  binding : int;
  local : int;
  locals : int StringMap.t;
  self : mod_scope;
  dependent_bindings : IntSet.t;
}

let new_state source =
  {
    source;
    binding = 0;
    local = 0;
    locals = StringMap.empty;
    self = StringMap.empty;
    dependent_bindings = IntSet.empty;
  }

type 'a resolver_result = Ok of 'a * resolver_state | Err of string
type value = ModuleItem of int | Local of int

let pure x s = Ok (x, s)
let err str s = Err str
let ( let* ) r f s = match r s with Ok (res, s) -> f res s | Err s -> Err s
let map f r s = match r s with Ok (res, s) -> Ok (f res, s) | Err s -> Err s

let traverse r list s =
  let g acc x =
    match acc with
    | Ok (res1, s) -> (
        match r x s with Ok (res2, s) -> Ok (res2 :: res1, s) | Err s -> Err s)
    | Err s -> Err s
  in
  let res = List.fold_left g (pure [] s) list in
  match res with Ok (res, s) -> Ok (List.rev res, s) | Err s -> Err s

let traverse_ r list s =
  let g acc x =
    match (acc, r x s) with
    | Ok (_, _), Ok (_, s) -> Ok ((), s)
    | Err s, _ -> Err s
    | _, Err s -> Err s
  in
  List.fold_left g (pure () s) list

type pat = Decl of int | Tuple of pat list

type expr =
  | Const of tok
  | Var of value
  | Bin of tok * expr * expr
  | App of expr * expr

type stmt = Binding of int * pat list * expr

let decl_binding tok s =
  let name = String.sub s.source tok.pos tok.len in
  match StringMap.find_opt name s.self with
  | Some binding -> Err ("binding already exists : " ^ name)
  | None ->
      Ok
        ( s.binding,
          {
            s with
            self = StringMap.add name s.binding s.self;
            binding = s.binding + 1;
          } )

let decl_stmt stmt =
  match stmt with Ast.Binding (tok, pats, expr) -> decl_binding tok

let decl_stmts stmts = traverse decl_stmt stmts
let scope r s = match r s with Ok (res, _) -> Ok (res, s) | Err s -> Err s

let decl_local tok s =
  let name = String.sub s.source tok.pos tok.len in
  Ok
    ( s.local,
      {
        s with
        locals = StringMap.add name s.local s.locals;
        local = s.local + 1;
      } )

let add_dependency binding s =
  Ok
    ((), { s with dependent_bindings = IntSet.add binding s.dependent_bindings })

let pop_dependencies s =
  let bindings = s.dependent_bindings in
  Ok (bindings, { s with dependent_bindings = IntSet.empty })

let get_name tok s =
  let name = String.sub s.source tok.pos tok.len in
  match (StringMap.find_opt name s.locals, StringMap.find_opt name s.self) with
  | Some local, _ -> Ok (Local local, s)
  | None, Some binding ->
      map (fun _ -> ModuleItem binding) (add_dependency binding) s
  | None, None -> Err ("name not found : " ^ name)

let rec pat p s =
  s
  |>
  match p with
  | Ast.Decl tok ->
      let* local = decl_local tok in
      pure (Decl local)
  | Ast.Tuple pats -> map (fun pats -> Tuple pats) (traverse pat pats)

let rec expr e s =
  s
  |>
  match e with
  | Ast.Const tok -> pure (Const tok)
  | Ast.Var tok -> map (fun v -> Var v) (get_name tok)
  | Ast.Bin (tok, lexpr, rexpr) ->
      let* lexpr = expr lexpr in
      let* rexpr = expr rexpr in
      pure (Bin (tok, lexpr, rexpr))
  | Ast.App (callee, arg) ->
      let* callee = expr callee in
      let* arg = expr arg in
      pure (App (callee, arg))

let stmt (binding, stmt) =
  match stmt with
  | Ast.Binding (tok, ps, e) ->
      scope
        (let* ps = traverse pat ps in
         let* e = expr e in
         let* deps = pop_dependencies in
         pure (Binding (binding, ps, e), deps))

let r_show_tok tok s = Ok (String.sub s.source tok.pos tok.len, s)

let show_bindings ast_stmts bindings =
  let name ast_stmt =
    match ast_stmt with Ast.Binding (tok, pats, expr) -> r_show_tok tok
  in
  map
    (fun names ->
      List.map
        (fun (name, binding) -> name ^ " : " ^ Int.to_string binding)
        (List.combine names bindings))
    (traverse name ast_stmts)
  |> map (String.concat "\n")

let show_dependencies stmts bindings =
  List.map
    (fun ((stmt, deps), binding) ->
      Int.to_string binding ^ " -> ["
      ^ String.concat ", " (List.map Int.to_string (IntSet.to_list deps))
      ^ "]")
    (List.combine stmts bindings)
  |> String.concat "\n"

let stmts stmts =
  let* bindings = decl_stmts stmts in
  let* stmts' = traverse stmt (List.combine bindings stmts) in
  let* str1 = show_bindings stmts bindings in
  let str2 = show_dependencies stmts' bindings in
  pure (str1, str2)
