
open Syntax
open Common

let rec get_env_struct (eff_defs : f_ENV) ({Zoo.data=exp; loc} : expr) (p_fun : string option) : env_struct list =
  match exp with
  | Var (_, x) -> []
  | Times (e1, e2) -> get_env_struct eff_defs e1 p_fun @ get_env_struct eff_defs e2 p_fun
  | Plus (e1, e2) -> get_env_struct eff_defs e1 p_fun @ get_env_struct eff_defs e2 p_fun
  | Minus (e1, e2) -> get_env_struct eff_defs e1 p_fun @ get_env_struct eff_defs e2 p_fun
  | Equal (e1, e2) -> get_env_struct eff_defs e1 p_fun @ get_env_struct eff_defs e2 p_fun
  | Less (e1, e2) -> get_env_struct eff_defs e1 p_fun @ get_env_struct eff_defs e2 p_fun
  | Assign (e1, e2) -> get_env_struct eff_defs e1 p_fun @ get_env_struct eff_defs e2 p_fun
  | Deref e -> get_env_struct eff_defs e p_fun
  | If (e1, e2, e3) ->
      get_env_struct eff_defs e1 p_fun @ get_env_struct eff_defs e2 p_fun @ get_env_struct eff_defs e3 p_fun
  | Let (x, ty, e1, e2) ->
      get_env_struct eff_defs e1 p_fun @ get_env_struct eff_defs e2 p_fun
  | Decl (x, ty, e1, e2) ->
      get_env_struct eff_defs e1 p_fun @ get_env_struct eff_defs e2 p_fun
  | Handle (x, fname, exp_catch, exp_handle) ->
      get_env_struct eff_defs exp_catch p_fun @ get_env_struct eff_defs exp_handle p_fun
  | FullFun (x, es1, hs, tm_args, ty, es2, exp_body) ->
    (x, p_fun, (x, full_fun_to_tabs exp) :: tm_args @ gather_locals exp_body) :: get_env_struct eff_defs exp_body (Some x)
  | FullApply (exp, es, hs, exps) ->
      get_env_struct eff_defs exp p_fun @ List.fold_left (fun acc exp_iter -> get_env_struct eff_defs exp_iter p_fun @ acc) [] exps
  | Raise (h, es, hs, exps) ->
      List.fold_left (fun acc exp_iter -> get_env_struct eff_defs exp_iter p_fun @ acc) [] exps
  | Seq (e1, e2) ->
      get_env_struct eff_defs e1 p_fun @ get_env_struct eff_defs e2 p_fun
  | Int _  | Bool _  -> []

let rec field_to_string (field_name: name) (field_type : ty) : string =
  match field_type with
  | TInt | TBool | TMut _ -> Printf.sprintf "%s %s;" (ty_to_string field_type) field_name
  | TAbs (es, hs, ty_args, ty, es') ->
    ty_to_string field_type ^ ";" |> Str.global_replace (Str.regexp "PLACEHOLDER") field_name

let env_struct_to_string (name, p_fun, locals) : string =
  let locals = List.map (fun (x, ty) -> field_to_string x ty ^ "\n") locals in
  let locals = String.concat "" locals in
  let env_field, p_fun_env_typedef = match p_fun with
    (* This env_field is unused because top-level function doesn't need env.
       However, we still keep one for consistency. This is especially needed
       because the caller doesn't know if a function if top-level, and will
       always call with an even. *)
    | None -> "void *env;\n", ""
    | Some p_fun -> Printf.sprintf "%s_env_t *env;\n" name, 
                    Printf.sprintf "typedef %s_locals_t %s_env_t;\n" p_fun name
  in
    p_fun_env_typedef ^ "\n" ^
    spf "typedef struct %s_locals_t {\n" name ^
    env_field ^
    locals ^ "} " ^
    spf "%s_locals_t;\n" name