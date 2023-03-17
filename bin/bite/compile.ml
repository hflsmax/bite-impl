(** Bite compiler. *)

open Syntax
open Syntax.R
open Common

let compile_hvar hvar = "locals." ^ hvar

let can_be_returned exp =
  match exp with
  | Var _ | Int _ | Bool _ | Times _ | Plus _ | Minus _ | Equal _ | Less _ | Deref _ | FullApply _ | Raise _ -> true
  | Assign _ | If _ | Let _ | Decl _ | Handle _ | FullFun _ | Abort | Resume _ | Seq _-> false

(* Control-flow destination *)
type cf_dest =
  | Return
  | Abort
  | Continue (* Neither return or abort. *)

(* Compile an expression to a top-level and a list of functions *)
let rec compile (cf_dest : cf_dest) ((exp, ty, effs, attrs) as exp': R.expr) : string * string list =
  begin match exp with
    | Var (depth, x) -> 
      spf "locals.%s%s" (String.concat "" (List.init depth (fun _ -> "env->"))) x, []
    | Int k -> string_of_int k, []
    | Bool b -> string_of_bool b, []
    | Times (e1, e2) ->
      let e1', f1 = compile Continue e1 in
      let e2', f2 = compile Continue e2 in
      spf "({%s * %s;})" e1' e2', f1 @ f2
    | Plus (e1, e2) ->
      let e1', f1 = compile Continue e1 in
      let e2', f2 = compile Continue e2 in
      spf "({%s + %s;})" e1' e2', f1 @ f2
    | Minus (e1, e2) ->
      let e1', f1 = compile Continue e1 in
      let e2', f2 = compile Continue e2 in
      spf "({%s - %s;})" e1' e2', f1 @ f2
    | Equal (e1, e2) ->
      let e1', f1 = compile Continue e1 in
      let e2', f2 = compile Continue e2 in
      spf "({%s == %s;})" e1' e2', f1 @ f2
    | Less (e1, e2) ->
      let e1', f1 = compile Continue e1 in
      let e2', f2 = compile Continue e2 in
      spf "({%s < %s;})" e1' e2', f1 @ f2
    | Deref e ->
      let e', _ = compile Continue e in
      e', []
    | Assign (e1, e2) ->
      let e1', _ = compile Continue e1 in
      let e2', f2 = compile Continue e2 in
      spf "%s = %s;\n" e1' e2', f2
    | If (e1, e2, e3) ->
      let e1', f1 = compile Continue e1 in
      let e2', f2 = compile cf_dest e2 in
      let e3', f3 = compile cf_dest e3 in
      if cf_dest = Continue then
        spf "(%s ? %s : %s)" e1' e2' e3', f1 @ f2 @ f3
      else
        spf "if (%s) {\n%s;\n} else {\n%s;\n}" e1' e2' e3', f1 @ f2 @ f3
    | Let (x, _, e1, e2) ->
      let e1', f1 = compile Continue e1 in
      let e2', f2 = compile cf_dest e2 in
      spf "locals.%s = %s;\n%s;" x e1' e2', f1 @ f2
    | Decl (x, _, e1, e2) ->
      let e1', f1 = compile Continue e1 in
      let e2', f2 = compile cf_dest e2 in
      spf "locals.%s = %s;\n%s" x e1' e2', f1 @ f2
    | Handle (handler_var_name, (fname, fname_ty), exp_catch, exp_handle) ->
      let[@warning "-partial-match"] (FullFun (kind, x, es1, hs, tm_args, ty, es2, body_exp), _, _, _) = exp_catch in
        let exp_catch_code, f1 = compile Continue exp_catch in
        let exp_handle_code, f2 = compile cf_dest exp_handle in
        let exp_handle_code =
        begin
        match kind with
        | TailResumptive -> exp_handle_code
        | Abortive -> 
          if cf_dest = Continue then
            spf "(setjmp(locals.%s.jb) == 0 ? %s : %s)" handler_var_name exp_handle_code "jmpret"
          else
            spf "if (setjmp(locals.%s.jb) == 0) {\n%s;\n} else {\n%s;\n}" handler_var_name exp_handle_code "return jmpret;"
        | _ -> Zoo.error "Other handler kind not supported"
        end in
        spf "locals.%s = %s;\n" handler_var_name exp_catch_code ^
        exp_handle_code, f1 @ f2
    | FullFun (kind, x, es1, hs, tm_args, ty, es2, body_exp) ->
      let code_tm_args = 
        List.map (fun (arg_name, ty_arg) -> ty_to_string ty_arg ^ " " ^ arg_name) tm_args in
      let code_tm_args = 
        (if x <> "main" then ["void* env"] else []) @
        (if kind <> Lambda then ["jmp_buf jb"] else []) @
        code_tm_args in
      let code_hs = List.map (fun (h, _, _) -> "closure_t " ^ h) hs
      in
      let code_init = 
        spf "%s_locals_t locals;\n" x ^
        (if x <> "main" then spf "locals.%s.f_ptr = (void*)%s; locals.%s.env = env;\n" x x x ^ spf "locals.env = (%s_env_t*)env;\n" x else "") ^
        (fst (List.split tm_args) @ List.map fst3 hs
        |> List.map (fun arg_name -> spf "locals.%s = %s;\n" arg_name arg_name)
        |> String.concat "") in
      let cf_dest = match kind with
        | TailResumptive | Lambda -> Return
        | Abortive -> Abort
        | GeneralHandler -> Zoo.error "General handlers not supported" in
      let code_body, f1 = compile cf_dest body_exp in
      let this_fun = spf "%s %s(%s)\n{\n%s\n%s\n}\n" (ty_to_string ty) x (String.concat ", " (code_tm_args @ code_hs)) code_init code_body in
      Zoo.print_info "Compiling function %s and locals are %s\n" x (String.concat ", " (List.map fst (gather_free_vars exp')));
      spf "({locals.%s.f_ptr = (void*)%s;\n" x x ^
      (if List.length (gather_free_vars exp') = 0 then "" else spf "locals.%s.env = &locals;\n" x) ^
      spf "copy_closure(locals.%s);})" x,
      f1 @ [this_fun]
    | FullApply ((_, lhs_ty, _, _) as lhs, es, hs, exps) ->
      let lhs', f1 = compile Continue lhs in
      let exps', f2 = List.split (List.map (compile Continue) exps) in
      let handler_args = List.map (fun x -> compile_hvar @@ fst3 @@ x) hs in
      let args_code = (lhs' ^ ".env") :: exps' @ handler_args in
      spf "((%s)%s.f_ptr)(%s);" (tabs_to_string lhs_ty false) lhs' (String.concat ", " args_code), f1 @ List.concat f2
    | Raise ((_, _, hty) as h, es, hs, exps) ->
      let exps', f2 = List.split (List.map (compile Continue) exps) in
      let handler_code = compile_hvar @@ fst3 @@ h in
      let handler_args = List.map (fun x -> compile_hvar @@ fst3 @@ x) hs in
      let args_code = 
        (handler_code ^ ".env") :: 
        (handler_code ^ ".jb") :: 
        exps' @ handler_args in
      spf "((%s)%s.f_ptr)(%s)" (tabs_to_string hty true) handler_code (String.concat ", " args_code), List.concat f2
    | Resume e ->
      let e', f1 = compile Continue e in
      "", f1
    | Abort -> "", []
    | Seq (e1, e2) ->
      let e1', f1 = compile Continue e1 in
      let e2', f2 = compile cf_dest e2 in
      spf "\n%s;\n%s;" e1' e2', f1 @ f2
  end
  |> fun (code, fs) -> (if can_be_returned exp then 
    begin
    match cf_dest with
    | Return -> (if (attrs.isRecursiveCall) then "__attribute__((musttail))" else "") ^ "return " ^ code ^ ";"
    | Abort -> spf "jmpret = %s;\nlongjmp(jb, 1);\n" code
    | Continue -> code
    end else code), fs
