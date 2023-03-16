(** Bite compiler. *)

open Syntax
open Syntax.R
open Common

let compile_hvar hvar = "locals." ^ hvar

let can_be_returned exp =
  match exp with
  | Var _ | Int _ | Bool _ | Times _ | Plus _ | Minus _ | Equal _ | Less _ | Deref _ | FullApply _ | Raise _ -> true
  | Assign _ | If _ | Let _ | Decl _ | Handle _ | FullFun _ | Handler _ | Abort | Resume _ | Seq _-> false

(* Compile an expression to a top-level and a list of functions *)
let rec compile (to_return : bool) ((exp, ty, effs, attrs) as exp': R.expr) : string * string list =
  begin match exp with
    | Var (depth, x) -> 
      spf "locals.%s%s" (String.concat "" (List.init depth (fun _ -> "env->"))) x, []
    | Int k -> string_of_int k, []
    | Bool b -> string_of_bool b, []
    | Times (e1, e2) ->
      let e1', f1 = compile false e1 in
      let e2', f2 = compile false e2 in
      spf "({%s * %s;})" e1' e2', f1 @ f2
    | Plus (e1, e2) ->
      let e1', f1 = compile false e1 in
      let e2', f2 = compile false e2 in
      spf "({%s + %s;})" e1' e2', f1 @ f2
    | Minus (e1, e2) ->
      let e1', f1 = compile false e1 in
      let e2', f2 = compile false e2 in
      spf "({%s - %s;})" e1' e2', f1 @ f2
    | Equal (e1, e2) ->
      let e1', f1 = compile false e1 in
      let e2', f2 = compile false e2 in
      spf "({%s == %s;})" e1' e2', f1 @ f2
    | Less (e1, e2) ->
      let e1', f1 = compile false e1 in
      let e2', f2 = compile false e2 in
      spf "({%s < %s;})" e1' e2', f1 @ f2
    | Deref e ->
      let e', _ = compile false e in
      e', []
    | Assign (e1, e2) ->
      let e1', _ = compile false e1 in
      let e2', f2 = compile false e2 in
      spf "%s = %s;\n" e1' e2', f2
    | If (e1, e2, e3) ->
      let e1', f1 = compile false e1 in
      let e2', f2 = compile to_return e2 in
      let e3', f3 = compile to_return e3 in
      if to_return then
        spf "if (%s) {\n%s;\n} else {\n%s;\n}" e1' e2' e3', f1 @ f2 @ f3
      else
        spf "(%s ? %s : %s)" e1' e2' e3', f1 @ f2 @ f3
    | Let (x, _, e1, e2) ->
      let e1', f1 = compile false e1 in
      let e2', f2 = compile to_return e2 in
      spf "locals.%s = %s;\n%s;" x e1' e2', f1 @ f2
    | Decl (x, _, e1, e2) ->
      let e1', f1 = compile false e1 in
      let e2', f2 = compile to_return e2 in
      spf "locals.%s = %s;\n%s" x e1' e2', f1 @ f2
    | Handle (handler_var_name, (fname, fname_ty), exp_catch, exp_handle) ->
        let exp_catch_code, f1 = compile false exp_catch in
        let exp_handle_code, f2 = compile to_return exp_handle in
        spf "locals.%s = %s;\n" handler_var_name exp_catch_code ^
        exp_handle_code, f1 @ f2
    | FullFun (x, es1, hs, tm_args, ty, es2, body_exp) ->
      let code_tm_args = 
        List.map (fun (arg_name, ty_arg) -> ty_to_string ty_arg ^ " " ^ arg_name) tm_args in
      let code_tm_args = if x <> "main" then "void* env" :: code_tm_args else code_tm_args in
      let code_hs = List.map (fun (h, _, _) -> "closure_t " ^ h) hs
      in
      let code_init = 
        spf "%s_locals_t locals;\n" x ^
        (if x <> "main" then spf "locals.%s = (closure_t){%s, env};\n" x x ^ "locals.env = env;\n" else "") ^
        (fst (List.split tm_args) @ List.map fst3 hs
        |> List.map (fun arg_name -> spf "locals.%s = %s;\n" arg_name arg_name)
        |> String.concat "") in
      let code_body, f1 = compile true body_exp in
      let this_fun = spf "%s %s(%s)\n{\n%s\n%s\n}\n" (ty_to_string ty) x (String.concat ", " (code_tm_args @ code_hs)) code_init code_body in
      spf "({locals.%s.f_ptr = %s;\n" x x ^
      (if List.length (gather_free_vars exp') = 0 then "" else spf "locals.%s.env = &locals;\n" x) ^
      spf "copy_closure(locals.%s);})" x,
      f1 @ [this_fun]
    | Handler (k, f) ->
      let f_code, f1 = compile false f in
      f_code, f1
    | FullApply ((_, lhs_ty, _, _) as lhs, es, hs, exps) ->
      let lhs', f1 = compile false lhs in
      let exps', f2 = List.split (List.map (compile false) exps) in
      let handler_args = List.map (fun x -> compile_hvar @@ fst3 @@ x) hs in
      let args_code = (lhs' ^ ".env") :: exps' @ handler_args in
      spf "((%s)%s.f_ptr)(%s);" (tabs_to_string lhs_ty) lhs' (String.concat ", " args_code), f1 @ List.concat f2
    | Raise ((_, _, hty) as h, es, hs, exps) ->
      let exps', f2 = List.split (List.map (compile false) exps) in
      let handler_code = compile_hvar @@ fst3 @@ h in
      let handler_args = List.map (fun x -> compile_hvar @@ fst3 @@ x) hs in
      let args_code = (handler_code ^ ".env") :: exps' @ handler_args in
      spf "((%s)%s.f_ptr)(%s)" (tabs_to_string hty) handler_code (String.concat ", " args_code), List.concat f2
    | Resume e ->
      let e', f1 = compile false e in
      "", f1
    | Abort -> "", []
    | Seq (e1, e2) ->
      let e1', f1 = compile false e1 in
      let e2', f2 = compile to_return e2 in
      spf "\n%s;\n%s;" e1' e2', f1 @ f2
  end
  |> fun (code, fs) -> (if to_return && (can_be_returned exp) then "return " ^ code else code), fs
