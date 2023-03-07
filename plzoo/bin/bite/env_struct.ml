
open Syntax
open Common

let rec get_env_struct (eff_defs : f_ENV) (exp : expr) : env_struct list =
  let rec get_env_struct' (eff_defs : f_ENV) ({Zoo.data=exp; loc} : expr) (slink : static_link) (p_fun : string option) : env_struct list =
    match exp with
    | Var (_, x) -> []
    | Times (e1, e2) -> get_env_struct' eff_defs e1 slink p_fun @ get_env_struct' eff_defs e2 slink p_fun
    | Plus (e1, e2) -> get_env_struct' eff_defs e1 slink p_fun @ get_env_struct' eff_defs e2 slink p_fun
    | Minus (e1, e2) -> get_env_struct' eff_defs e1 slink p_fun @ get_env_struct' eff_defs e2 slink p_fun
    | Equal (e1, e2) -> get_env_struct' eff_defs e1 slink p_fun @ get_env_struct' eff_defs e2 slink p_fun
    | Less (e1, e2) -> get_env_struct' eff_defs e1 slink p_fun @ get_env_struct' eff_defs e2 slink p_fun
    | Assign (e1, e2) -> get_env_struct' eff_defs e1 slink p_fun @ get_env_struct' eff_defs e2 slink p_fun
    | Deref e -> get_env_struct' eff_defs e slink p_fun
    | If (e1, e2, e3) ->
       get_env_struct' eff_defs e1 slink p_fun @ get_env_struct' eff_defs e2 slink p_fun @ get_env_struct' eff_defs e3 slink p_fun
    | Let (x, ty, e1, e2) ->
       get_env_struct' eff_defs e1 slink p_fun @ get_env_struct' eff_defs e2 slink p_fun
    | Decl (x, ty, e1, e2) ->
       get_env_struct' eff_defs e1 slink p_fun @ get_env_struct' eff_defs e2 slink p_fun
    | Handle (x, fname, exp_catch, exp_handle) ->
       get_env_struct' eff_defs exp_catch slink p_fun @ get_env_struct' eff_defs exp_handle slink p_fun
    | FullFun (x, es1, hs, tm_args, ty, es2, exp) ->
      let ty_args = List.map snd tm_args in
        let locals = (x, TAbs (es1, hs, ty_args, ty, es2)) :: tm_args @ gather_locals eff_defs exp in
          (x, p_fun, tm_args @ gather_locals eff_defs exp) :: get_env_struct' eff_defs exp (locals :: slink) (Some x)
    | FullApply (exp, es, hs, exps) ->
       get_env_struct' eff_defs exp slink p_fun @ List.fold_left (fun acc exp_iter -> get_env_struct' eff_defs exp_iter slink p_fun @ acc) [] exps
    | Raise (h, es, hs, exps) ->
        List.fold_left (fun acc exp_iter -> get_env_struct' eff_defs exp_iter slink p_fun @ acc) [] exps
    | Seq (e1, e2) ->
        get_env_struct' eff_defs e1 slink p_fun @ get_env_struct' eff_defs e2 slink p_fun
    | Int _  | Bool _  -> []
  in
    get_env_struct' eff_defs exp (gather_locals eff_defs exp :: []) None

let rec ty_to_clang_ty (ty : ty) : Clang.Type.t =
  match ty with
  | TInt -> Clangml_helper.int
  | TBool -> Clangml_helper.bool
  | TMut ty' -> ty_to_clang_ty ty'
  | TAbs (es, hs, ty_args, ty, es2) -> Clangml_helper.bool

let env_struct_to_ast (env_struct : env_struct) : Clang.Ast.decl =
  let (name, p_fun, locals) = env_struct in
  let locals_ast = List.map (fun (x, ty) -> Clangml_helper.field x (ty_to_clang_ty ty)) locals in
    match p_fun with
      | None -> Clangml_helper.record_decl name locals_ast
      | Some p_fun_name -> 
          Clangml_helper.record_decl name 
            ((Clangml_helper.field p_fun_name (Clangml_helper.record (Clang.Ast.ident_ref (Clang.Ast.IdentifierName "ABC")))) :: locals_ast)

         

let env_structs_to_ast (env_structs : env_struct list) : Clang.Ast.translation_unit =
  Clangml_helper.translation_unit (List.map env_struct_to_ast env_structs)