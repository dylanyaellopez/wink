module IntMap = Map.Make (Int)
open Token

type value =
  | Pi of value * (value -> value)
  | Lam of (value -> value)
  | Universe of int
  | Meta of int
  | IntLit of int
  | Neutral of (Resolver.value, int) Either.t * value list

type value_info = value * value

type analysis_state = {
  source : string;
  binding_ctx : value_info IntMap.t;
  local_ctx : value_info IntMap.t;
  meta_subs : value IntMap.t;
  temp_var : int;
}

type 'a analysis_result = Ok of 'a * analysis_state | Err of string

let pure x s = Ok (x, s)
let err str s = Err str
let ( let* ) a f s = match a s with Ok (res, s) -> f res s | Err s -> Err s
let map f a s = match a s with Ok (res, s) -> Ok (f res, s) | Err s -> Err s

let temp_scope a s =
  match a s with
  | Ok (res, s') -> Ok (res, { s' with temp_var = s.temp_var })
  | Err s -> Err s

let new_temp s = Ok (s.temp_var, { s with temp_var = s.temp_var + 1 })

let traverse a list s =
  let g acc x =
    match acc with
    | Ok (res1, s) -> (
        match a x s with Ok (res2, s) -> Ok (res2 :: res1, s) | Err s -> Err s)
    | Err s -> Err s
  in
  let res = List.fold_left g (pure [] s) list in
  match res with Ok (res, s) -> Ok (List.rev res, s) | Err s -> Err s

let meta_sub meta s =
  match IntMap.find_opt meta s.meta_subs with
  | Some x -> Ok (Some x, s)
  | None -> Ok (None, s)

let add_meta_sub meta v s =
  Ok ((), { s with meta_subs = IntMap.add meta v s.meta_subs })

let from_bindings_ctx value s =
  match IntMap.find_opt value s.binding_ctx with
  | Some (value, ty) -> Ok ((value, ty), s)
  | None ->
      Err
        "this should not appear. this means a dependency was not analyzed \
         before a dependent, should be impossible with sccs."

let from_locals_ctx value s =
  match IntMap.find_opt value s.binding_ctx with
  | Some (value, ty) -> Ok ((value, ty), s)
  | None -> Err "this should not appear. issue with locals."

let traverse_ a list s =
  let g acc x =
    match acc with
    | Ok (_, s) -> (
        match a x s with Ok (_, s) -> Ok ((), s) | Err s -> Err s)
    | Err s -> Err s
  in
  List.fold_left g (pure () s) list

let parse_int tok s =
  match int_of_string_opt (String.sub s.source tok.pos tok.len) with
  | Some int -> Ok (int, s)
  | None -> Err "could not parse int"

let rec unify ty1 ty2 (s : analysis_state) : 'a analysis_result =
  s
  |>
  match (ty1, ty2) with
  | Meta x, _ -> (
      let* sub = meta_sub x in
      match sub with Some v -> unify v ty2 | None -> add_meta_sub x ty2)
  | _, Meta x -> (
      let* sub = meta_sub x in
      match sub with Some v -> unify ty1 v | None -> add_meta_sub x ty1)
  | IntLit x1, IntLit x2 ->
      if x1 = x2 then pure ()
      else
        err
          ("ints " ^ Int.to_string x1 ^ "did not unify with " ^ Int.to_string x2)
  | Neutral (v1, vs1), Neutral (v2, vs2) ->
      if v1 = v2 then unify_two_lists vs1 vs2
      else err "neutrals did not have same base type"
  | Pi (a1, b1), Pi (a2, b2) ->
      let* () = unify a1 a2 in
      temp_scope
        (let* x = new_temp in
         unify
           (b1 (Neutral (Either.Right x, [])))
           (b2 (Neutral (Either.Right x, []))))
  | Universe n1, Universe n2 ->
      if n1 <= n2 then pure ()
      else
        err
          ("expected at least type universe " ^ Int.to_string n2
         ^ " or higher but got " ^ Int.to_string n1)
  | _ -> err "doesnt unify"

and unify_two_lists xs ys s =
  s
  |>
  match (xs, ys) with
  | x :: xs, y :: ys ->
      let* () = unify x y in
      unify_two_lists xs ys
  | [], [] -> pure ()
  | _ -> err "args were not the same size"

let rec infer term s =
  s
  |>
  match term with
  | Resolver.Const ({ ty = Number } as tok) -> err "add proper lang items"
  | Resolver.Const _ -> err "non const token in const expr should not happen"
  | Resolver.Var (Resolver.ModuleItem binding) ->
      map (fun (_, t) -> t) (from_bindings_ctx binding)
  | Resolver.Var (Resolver.Local local) ->
      map (fun (_, t) -> t) (from_locals_ctx local)
  | Resolver.App (callee, x) -> (
      let* v1 = eval callee in
      let* v2 = eval x in
      let* t1 = infer callee in
      let* t2 = infer x in
      match t1 with
      | Pi (a, b) ->
          let* () = unify t2 a in
          let* v = new_temp in
          pure (b (Neutral (Either.Right v, [])))
      | _ -> err "not a function")

and eval term s =
  s
  |>
  match term with
  | Resolver.Const ({ ty = Number } as tok) ->
      map (fun x -> IntLit x) (parse_int tok)
  | Resolver.Const _ -> err "non const token in const expr should not happen"
  | Resolver.Var (Resolver.ModuleItem binding) ->
      map (fun (v, _) -> v) (from_bindings_ctx binding)
  | Resolver.Var (Resolver.Local local) ->
      map (fun (v, _) -> v) (from_locals_ctx local)
  | Resolver.App (callee, x) -> (
      let* v1 = eval callee in
      let* v2 = eval x in
      let* t1 = infer callee in
      let* t2 = infer x in
      match t2 with
      | Pi (a, b) -> (
          let* () = unify t2 a in
          match v1 with
          | Lam f -> pure (f v2)
          | Neutral (e, xs) -> pure (Neutral (e, xs @ [ v2 ]))
          | _ -> err "should not have gotten past type checking")
      | _ -> err "not a function")
