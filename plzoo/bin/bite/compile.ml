(** Bite compiler. *)

open Syntax
open Common

let compile_hvar hvar = "locals." ^ hvar

(* Compile an expression to a top-level and a list of functions *)
let rec compile {Zoo.data=e'; _} : string * string list =
  match e' with
    | Var (depth, x) -> 
      spf "locals.%s%s" (String.concat "" (List.init depth (fun _ -> "env->"))) x, []
    | Int k -> string_of_int k, []
    | Bool b -> string_of_bool b, []
    | Times (e1, e2) ->
      let e1', f1 = compile e1 in
      let e2', f2 = compile e2 in
      spf "({%s * %s;})" e1' e2', f1 @ f2
    | Plus (e1, e2) ->
      let e1', f1 = compile e1 in
      let e2', f2 = compile e2 in
      spf "({%s + %s;})" e1' e2', f1 @ f2
    | Minus (e1, e2) ->
      let e1', f1 = compile e1 in
      let e2', f2 = compile e2 in
      spf "({%s - %s;})" e1' e2', f1 @ f2
    | Equal (e1, e2) ->
      let e1', f1 = compile e1 in
      let e2', f2 = compile e2 in
      spf "({%s == %s;})" e1' e2', f1 @ f2
    | Less (e1, e2) ->
      let e1', f1 = compile e1 in
      let e2', f2 = compile e2 in
      spf "({%s < %s;})" e1' e2', f1 @ f2
    | Deref e ->
      let e', _ = compile e in
      spf "(%s)" e', []
    | Assign (e1, e2) ->
      let e1', _ = compile e1 in
      let e2', f2 = compile e2 in
      spf "locals.%s = %s;\n" e1' e2', f2
    | If (e1, e2, e3) ->
      let e1', f1 = compile e1 in
      let e2', f2 = compile e2 in
      let e3', f3 = compile e3 in
      spf "(%s ? ({%s;}) : ({%s;}))" e1' e2' e3', f1 @ f2 @ f3
    | Let (x, _, e1, e2) ->
      let e1', f1 = compile e1 in
      let e2', f2 = compile e2 in
      spf "locals.%s = %s;\n%s" x e1' e2', f1 @ f2
    | Decl (x, _, e1, e2) ->
      let e1', f1 = compile e1 in
      let e2', f2 = compile e2 in
      spf "locals.%s = %s;\n%s" x e1' e2', f1 @ f2
    | Handle (handler_var_name, (fname, fname_ty), ({Zoo.data=exp_catch'; loc} as exp_catch), exp_handle) ->
      begin
      match exp_catch' with
      | FullFun _ ->
        let exp_catch_code, f1 = compile exp_catch in
        let exp_handle_code, f2 = compile exp_handle in
        spf "locals.%s = %s;\n" handler_var_name exp_catch_code ^
        exp_handle_code, f1 @ f2
      | _ -> Zoo.error "Handler must be of function type.\n"
      end
    | FullFun (x, es1, hs, tm_args, ty, es2, exp) ->
      let code_tm_args = 
        "void* env" :: List.map (fun (arg_name, ty_arg) -> ty_to_string ty_arg ^ " " ^ arg_name) tm_args in
      let code_hs = List.map (fun (h, _) -> "closure_t* " ^ h) hs
      in
      let code_init = 
        spf "%s_locals_t locals;\n" x ^
        spf "locals.%s.f_ptr = %s;\n" x x ^
        spf "locals.%s.env = env;\n" x ^
        ("env" :: fst (List.split tm_args) @ fst (List.split hs) 
        |> (List.map (fun arg_name -> spf "locals.%s = %s;\n" arg_name arg_name)) 
        |> String.concat "") in
      let code_body, f1 = compile exp in
      let this_fun = spf "%s %s(%s)\n{\n%sreturn ({\n%s\n;});}\n" (ty_to_string ty) x (String.concat ", " (code_tm_args @ code_hs)) code_init code_body in
      spf "({locals.%s.f_ptr = %s;\n" x x ^
      spf "locals.%s.env = &locals;\n" x ^
      spf "&locals.%s;})" x,
      this_fun :: f1
    | FullApply ((lhs, lhs_ty), es, hs, exps) ->
      let lhs', f1 = compile lhs in
      let exps', f2 = List.split (List.map compile exps) in
      let handler_args = List.map compile_hvar hs in
      let args_code = (lhs' ^ ".env") :: exps' @ handler_args in
      spf "(%s.f_ptr)(%s)" lhs' (String.concat ", " args_code), f1 @ List.concat f2
    | Raise (h, es, hs, exps) ->
      let exps', f2 = List.split (List.map compile exps) in
      let handler_args = List.map (fun h -> [h; h ^ "_env"]) hs |> List.concat in
      let args_code = (h ^ "_env") :: exps' @ handler_args in
      spf "(%s)(%s)" h (String.concat ", " args_code), List.concat f2
    | Seq (e1, e2) ->
      let e1', f1 = compile e1 in
      let e2', f2 = compile e2 in
      spf "\n%s;\n%s" e1' e2', f1 @ f2

(* Top level can only be function definition and application *)
let rec compile_toplevel {Zoo.data=e'; _} : string =
  match e' with
  | Let (x, ty, e1, e2) ->
    let e1', f1 = compile e1 in
    let e2' = compile_toplevel e2 in
    spf "locals.%s = %s;\n%s" x e1' e2'
  | FullApply (lhs, es, hs, exps) ->
    ""
  | _ -> Zoo.error "Toplevel expression must be a let binding of function or application.\n"