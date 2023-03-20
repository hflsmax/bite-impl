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
      begin
      match e1 with
      | (FullFun (_, fun_name, _, _, _, _, _, _), _, _, _) ->
        spf "locals.%s_fptr = (void*)%s;\n" x fun_name ^
        spf "locals.%s_env = &locals;\n" x ^
        e2' ^ "\n"
      | _ -> spf "locals.%s = %s;\n%s;" x e1' e2'
      end, f1 @ f2
    | Decl (x, _, e1, e2) ->
      let e1', f1 = compile Continue e1 in
      let e2', f2 = compile cf_dest e2 in
      spf "locals.%s = %s;\n%s" x e1' e2', f1 @ f2
    | Handle (handler_var_name, (fname, fname_ty), exp_catch, exp_handle) ->
      let[@warning "-partial-match"] (FullFun (kind, fun_name, _, _, _, _, _, _), _, _, _) = exp_catch in
        let exp_catch_code, f1 = compile Continue exp_catch in
        let exp_handle_code, f2 = compile cf_dest exp_handle in
        let exp_handle_code =
        begin
        match kind with
        | TailResumptive -> exp_handle_code
        | Abortive -> 
          if cf_dest = Continue then
            spf "(setjmp(*locals.%s_jb) == 0 ? ({%s;}) : ({%s;}))" handler_var_name exp_handle_code "jmpret"
          else
            spf "if (setjmp(*locals.%s_jb) == 0) {\n%s;\n} else {\n%s;\n}" handler_var_name exp_handle_code "return jmpret;"
        | _ -> Zoo.error "Other handler kind not supported"
        end in
        (if cf_dest = Continue then "({" else "") ^
        spf "locals.%s_fptr = (void*)%s;\n" handler_var_name fun_name ^
        spf "locals.%s_env = &locals;\n" handler_var_name ^
        spf "jmp_buf _%s_jb;\n" handler_var_name ^
        spf "locals.%s_jb = &_%s_jb;\n" handler_var_name handler_var_name ^
        exp_handle_code ^
        (if cf_dest = Continue then ";})" else "")
        , f1 @ f2
    | FullFun (kind, x, es1, hs, tm_args, ty, es2, body_exp) ->
      let code_tm_args = 
        (if x <> "main" then ["void* env"] else []) @
        (if kind <> Lambda then ["jmp_buf jb"] else []) @
        List.map (fun (arg_name, arg_ty) -> match arg_ty with
                                            | TAbs _ -> spf "void* %s_fptr, void* %s_env" arg_name arg_name 
                                            | _ -> ty_to_string arg_ty ^ " " ^ arg_name) tm_args in
      let code_hs = List.map (fun (h, _, _) -> spf "void *%s_fptr, void *%s_env, jmp_buf *%s_jb" h h h) hs
      in
      let code_init = 
        spf "%s_locals_t locals;\n" x ^
        (if x <> "main" then spf "locals.env = (%s_env_t*)env;\n" x else "") ^
        (tm_args
          |> List.map (fun (arg_name, arg_ty) -> match arg_ty with 
                                                 | TAbs _ -> spf "locals.%s_fptr = %s_fptr;\nlocals.%s_env = %s_env;\n" arg_name arg_name arg_name arg_name 
                                                 | _ -> spf "locals.%s = %s;\n" arg_name arg_name)
          |> String.concat "") ^
        (List.map fst3 hs
          |> List.map (fun h -> spf "locals.%s_fptr = %s_fptr;\nlocals.%s_env = %s_env;\nlocals.%s_jb = %s_jb;\n" h h h h h h)
          |> String.concat "") in
      let cf_dest = match kind with
        | TailResumptive | Lambda -> Return
        | Abortive -> Abort
        | GeneralHandler -> Zoo.error "General handlers not supported" in
      let code_body, f1 = compile cf_dest body_exp in
      let this_fun = spf "%s %s(%s)\n{\n%s\n%s\n}\n" (ty_to_string ty) x (String.concat ", " (code_tm_args @ code_hs)) code_init code_body in
      "CURRENTLY ONLY SUPPORT FUNCTIONS ASSIGNED TO A VARIABLE OR A HANDLER",
      f1 @ [this_fun]
    | FullApply ((lhs_name, lhs_ty, _, _) as lhs, es, hs, exps) ->
      let lhs', f1 = compile Continue lhs in
      let exps', f2 = List.split (List.map (fun ((exp, ty, _, _) as rexp) -> let exp_code, fs = compile Continue rexp in match ty with
                                                                                                                          | TAbs _ -> spf "%s_fptr, %s_env" exp_code exp_code, fs
                                                                                                                          | _ -> exp_code, fs) exps) in
      let handler_args = List.map (fun x -> (fun y -> spf "locals.%s_fptr, locals.%s_env, locals.%s_jb" y y y) @@ fst3 @@ x) hs in
      let args_code = if attrs.isBuildIn then exps' @ handler_args else if attrs.isRecursiveCall then ("locals.env") :: exps' @ handler_args else (lhs' ^ "_env") :: exps' @ handler_args in
      let lhs_code = if attrs.isBuildIn then let[@warning "-partial-match"] Var (_, x) = lhs_name in x else if Option.is_some attrs.topLevelFunctionName then Option.get(attrs.topLevelFunctionName) else spf "((%s)%s_fptr)" (tabs_to_string lhs_ty false) lhs' in
      spf "%s(%s)" lhs_code (String.concat ", " args_code), f1 @ List.concat f2
    | Raise ((_, _, hty) as h, es, hs, exps) ->
      let exps', f2 = List.split (List.map (fun ((exp, ty, _, _) as rexp) -> let exp_code, fs = compile Continue rexp in match ty with
                                                                                                                          | TAbs _ -> spf "%s_fptr, %s_env" exp_code exp_code, fs
                                                                                                                          | _ -> exp_code, fs) exps) in
      let handler_code = compile_hvar @@ fst3 @@ h in
      let handler_args = List.map (fun x -> (fun y -> spf "locals.%s_fptr, locals.%s_env, locals.%s_jb" y y y) @@ fst3 @@ x) hs in
      let args_code = (handler_code ^ "_env") :: (handler_code ^ "_jb") :: exps' @ handler_args in
      let lhs_code = if Option.is_some attrs.topLevelFunctionName then Option.get(attrs.topLevelFunctionName) else spf "((%s)%s_fptr)" (tabs_to_string hty true) handler_code in
      spf "%s(%s)" lhs_code (String.concat ", " args_code), List.concat f2
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
