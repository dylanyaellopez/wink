open Resolver

let load_prims =
  let* i8 = decl_binding_str "I8" in
  let* i16 = decl_binding_str "I16" in
  let* i32 = decl_binding_str "I32" in
  pop_bindings

let import_foreign r =
  let* foreign = r in
  let* () = full_import foreign in
  let* _ = pop_dependencies in
  pure ()
