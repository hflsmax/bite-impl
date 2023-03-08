
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
  | FullFun (x, es1, hs, tm_args, ty, es2, exp) ->
    (x, p_fun, tm_args @ gather_locals eff_defs exp) :: get_env_struct eff_defs exp (Some x)
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
    ty_to_string field_type |> Str.global_replace (Str.regexp "PLACEHOLDER") field_name

let env_struct_to_string (name, p_fun, locals) : string =
  let locals = List.map (fun (x, ty) -> field_to_string x ty ^ "\n") locals in
  let locals = String.concat "" locals in
  let env_field = match p_fun with
    | None -> ""
    | Some p_fun -> Printf.sprintf "%s_locals *env;\n" p_fun
  in
    Printf.sprintf "typedef struct %s_locals {\n%s%s} %s_locals;\n" name env_field locals name