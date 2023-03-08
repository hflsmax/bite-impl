(** Bite compiler. *)

open Syntax
open Common

let compile_hvar (hvar : hvar) : string = "locals." ^ hvar

(* Compile an expression to a top-level and a list of functions *)
let rec compile (eff_defs : f_ENV) {Zoo.data=e'; _} : string * string list =
  match e' with
    | Var (depth, x) -> 
      Printf.sprintf "locals.%s%s" (String.concat "" (List.init depth (fun _ -> "env->"))) x, []
    | Int k -> string_of_int k, []
    | Bool b -> string_of_bool b, []
    | Times (e1, e2) ->
      let e1', f1 = compile eff_defs e1 in
      let e2', f2 = compile eff_defs e2 in
      Printf.sprintf "({%s * %s})" e1' e2', f1 @ f2
    | Plus (e1, e2) ->
      let e1', f1 = compile eff_defs e1 in
      let e2', f2 = compile eff_defs e2 in
      Printf.sprintf "({%s + %s})" e1' e2', f1 @ f2
    | Minus (e1, e2) ->
      let e1', f1 = compile eff_defs e1 in
      let e2', f2 = compile eff_defs e2 in
      Printf.sprintf "({%s - %s})" e1' e2', f1 @ f2
    | Equal (e1, e2) ->
      let e1', f1 = compile eff_defs e1 in
      let e2', f2 = compile eff_defs e2 in
      Printf.sprintf "({%s == %s})" e1' e2', f1 @ f2
    | Less (e1, e2) ->
      let e1', f1 = compile eff_defs e1 in
      let e2', f2 = compile eff_defs e2 in
      Printf.sprintf "({%s < %s})" e1' e2', f1 @ f2
    | Deref e ->
      let e', _ = compile eff_defs e in
      Printf.sprintf "(%s)" e', []
    | Assign (e1, e2) ->
      let e1', _ = compile eff_defs e1 in
      let e2', f2 = compile eff_defs e2 in
      Printf.sprintf "locals.%s = %s;\n" e1' e2', f2
    | If (e1, e2, e3) ->
      let e1', f1 = compile eff_defs e1 in
      let e2', f2 = compile eff_defs e2 in
      let e3', f3 = compile eff_defs e3 in
      Printf.sprintf "(%s ? ({%s}) : ({%s}))" e1' e2' e3', f1 @ f2 @ f3
    | Let (x, _, e1, e2) ->
      let e1', f1 = compile eff_defs e1 in
      let e2', f2 = compile eff_defs e2 in
      Printf.sprintf "locals.%s = %s;\n%s" x e1' e2', f1 @ f2
    | Decl (x, _, e1, e2) ->
      let e1', f1 = compile eff_defs e1 in
      let e2', f2 = compile eff_defs e2 in
      Printf.sprintf "locals.%s = %s;\n%s" x e1' e2', f1 @ f2
    | Handle (handler_var_name, fname, ({Zoo.data=exp_catch'; loc} as exp_catch), exp_handle) ->
      begin
      match exp_catch' with
      | FullFun _ ->
        let exp_catch_code, f1 = compile eff_defs exp_catch in
        let exp_handle_code, f2 = compile eff_defs exp_handle in
        (* TODO: use the string returned from compile FullFun *)
        Printf.sprintf "locals.%s = %s;\n%s" handler_var_name exp_catch_code exp_handle_code, f1 @ f2
      | _ -> Zoo.error "Handler must be of function type.\n"
      end
    | FullFun (x, es1, hs, tm_args, ty, es2, exp) ->
      let code_body, f1 = compile eff_defs exp in
      let code_init = 
        Printf.sprintf "%s_locals locals;\n" x ^
        String.concat "" ("locals.env = env;\n" :: List.map (fun (arg_name, _) -> Printf.sprintf "locals.%s = %s;\n" arg_name arg_name) tm_args) in
      let code_tm_args = "void* env" :: List.map (fun (arg_name, ty_arg) -> ty_to_string ty_arg ^ " " ^ arg_name) tm_args in
      let code_hs = List.map (fun (h, fname) -> Str.global_replace (Str.regexp "PLACEHOLDER") h (ty_to_string (List.assoc fname eff_defs))) hs in
      let this_fun = Printf.sprintf "%s %s(%s)\n{\n%sreturn ({\n%s\n});}\n" (ty_to_string ty) x (String.concat ", " (code_tm_args @ code_hs)) code_init code_body in
      "&" ^ x , this_fun :: f1
    | FullApply (exp, es, hs, exps) ->
      let exp', f1 = compile eff_defs exp in
      let exps', f2 = List.split (List.map (compile eff_defs) exps) in
      Printf.sprintf "(%s)(%s)" exp' (String.concat ", " (exps' @ List.map compile_hvar hs)), f1 @ List.concat f2
    | Raise (h, es, hs, exps) ->
      let exps', f2 = List.split (List.map (compile eff_defs) exps) in
      Printf.sprintf "(%s)(%s)" (compile_hvar h) (String.concat ", " (exps' @ List.map compile_hvar hs)), List.concat f2
    | Seq (e1, e2) ->
      let e1', f1 = compile eff_defs e1 in
      let e2', f2 = compile eff_defs e2 in
      Printf.sprintf "\n%s;\n%s" e1' e2', f1 @ f2
