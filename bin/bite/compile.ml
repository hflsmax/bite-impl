(** Bite compiler. *)

open Syntax
open Common
open Util

let compile_hvar hvar = "locals." ^ hvar

let can_be_returned exp =
  match exp with
  | Var _ | Int _ | Bool _ | Times _ | Plus _ | Minus _ | Equal _ | Less _
  | Deref _ | FullApply _ | Raise _ ->
      true
  | Assign _ | If _ | Let _ | Decl _ | Handle _ | FullFun _ | Resume _ | Seq _
    ->
      false

(* Compile an expression to a top-level and a list of functions *)
let compile exp : string =
  let global_code = ref "" in
  let fs = ref [] in
  let rec compile_rec ((exp, attrs) : expr) : string =
    (match exp with
    | Var x ->
        if attrs.isBuiltin then x
        else
          spf "locals.%s%s"
            (String.concat "" (List.init attrs.varDepth (fun _ -> "env->")))
            x
    | Int k -> string_of_int k
    | Bool b -> string_of_bool b
    | Times (e1, e2) ->
        let e1' = compile_rec e1 in
        let e2' = compile_rec e2 in
        spf "({%s * %s;})" e1' e2'
    | Plus (e1, e2) ->
        let e1' = compile_rec e1 in
        let e2' = compile_rec e2 in
        spf "({%s + %s;})" e1' e2'
    | Minus (e1, e2) ->
        let e1' = compile_rec e1 in
        let e2' = compile_rec e2 in
        spf "({%s - %s;})" e1' e2'
    | Equal (e1, e2) ->
        let e1' = compile_rec e1 in
        let e2' = compile_rec e2 in
        spf "({%s == %s;})" e1' e2'
    | Less (e1, e2) ->
        let e1' = compile_rec e1 in
        let e2' = compile_rec e2 in
        spf "({%s < %s;})" e1' e2'
    | Deref e ->
        let e' = compile_rec e in
        e'
    | Assign (e1, e2) ->
        let e1' = compile_rec e1 in
        let e2' = compile_rec e2 in
        spf "%s = %s;\n" e1' e2'
    | If (e1, e2, e3) ->
        let e1' = compile_rec e1 in
        let e2' = compile_rec e2 in
        let e3' = compile_rec e3 in
        if attrs.cfDest = Continue then spf "(%s ? %s : %s)" e1' e2' e3'
        else spf "if (%s) {\n%s;\n} else {\n%s;\n}" e1' e2' e3'
    | Let (x, e1, e2) -> (
        let e1' = compile_rec e1 in
        let e2' = compile_rec e2 in
        match e1 with
        | FullFun (fun_name, _, _, _, _, _, _), _ ->
            spf "locals.%s_fptr = (void*)%s;\n" x fun_name
            ^ spf "locals.%s_env = &locals;\n" x
            ^ e2' ^ "\n"
        | _ -> spf "locals.%s = %s;\n%s;" x e1' e2')
    | Decl (x, e1, e2) ->
        let e1' = compile_rec e1 in
        let e2' = compile_rec e2 in
        spf "locals.%s = %s;\n%s" x e1' e2'
    | Handle (handler_var_name, fname, exp_catch, exp_handle) ->
        let[@warning "-partial-match"] FullFun (fun_name, _, _, _, _, _, _), _ =
          exp_catch
        in
        let _ = compile_rec exp_catch in
        let exp_handle_code = compile_rec exp_handle in
        let exp_handle_code =
          match Option.get attrs.handlerKind with
          | TailResumptive -> exp_handle_code
          | Abortive ->
              let c1 =
                if attrs.isOptimizedSjlj then spf "%s_saved || " fun_name
                else ""
              in
              let c2 =
                if attrs.isOptimizedSjlj then
                  spf "locals.%s_jb = &%s_jb;\n" handler_var_name fun_name
                else
                  spf "jmp_buf _%s_jb;\nlocals.%s_jb = &_%s_jb;\n" fun_name
                    handler_var_name fun_name
              in
              let c3 =
                if attrs.isOptimizedSjlj then spf "%s_saved = true;\n" fun_name
                else ""
              in
              let _ =
                if attrs.isOptimizedSjlj then
                  global_code :=
                    spf "bool %s_saved = false;\njmp_buf %s_jb;\n" fun_name
                      fun_name
                    ^ !global_code
                else ()
              in
              if attrs.cfDest = Continue then
                spf "%s\n(%s_setjmp(locals.%s_jb) == 0 ? ({%s%s;}) : ({%s;}))"
                  c2 c1 handler_var_name c3 exp_handle_code "jmpret"
              else
                spf
                  "%s\n\
                   if (%s_setjmp(locals.%s_jb) == 0) {\n\
                   %s%s;\n\
                   } else {\n\
                   %s;\n\
                   }"
                  c2 c1 handler_var_name c3 exp_handle_code "return jmpret;"
          | Multishot | SingleShot -> exp_handle_code
        in
        (if attrs.cfDest = Continue then "({" else "")
        ^ spf "locals.%s_fptr = (void*)%s;\n" handler_var_name fun_name
        ^ spf "locals.%s_env = &locals;\n" handler_var_name
        ^ exp_handle_code
        ^ if attrs.cfDest = Continue then ";})" else ""
    | FullFun (x, es1, hs, tm_args, ty, es2, body_exp) ->
        let code_tm_args =
          (if x <> "main" then [ "void* env" ] else [])
          @ (if attrs.handlerKind <> None then [ "jmp_buf jb" ] else [])
          @ List.map
              (fun (arg_name, arg_ty) ->
                match arg_ty with
                | TAbs _ -> spf "void* %s_fptr, void* %s_env" arg_name arg_name
                | _ -> ty_to_string arg_ty ^ " " ^ arg_name)
              tm_args
        in
        let code_hs =
          List.map
            (fun (h, _) ->
              spf "void *%s_fptr, void *%s_env, jmp_buf *%s_jb" h h h)
            hs
        in
        let code_init =
          spf "%s_locals_t locals;\n" x
          ^ (if x <> "main" then spf "locals.env = (%s_env_t*)env;\n" x else "")
          ^ (tm_args
            |> List.map (fun (arg_name, arg_ty) ->
                   match arg_ty with
                   | TAbs _ ->
                       spf
                         "locals.%s_fptr = %s_fptr;\nlocals.%s_env = %s_env;\n"
                         arg_name arg_name arg_name arg_name
                   | _ -> spf "locals.%s = %s;\n" arg_name arg_name)
            |> String.concat "")
          ^ (List.map fst hs
            |> List.map (fun h ->
                   spf
                     "locals.%s_fptr = %s_fptr;\n\
                      locals.%s_env = %s_env;\n\
                      locals.%s_jb = %s_jb;\n"
                     h h h h h h)
            |> String.concat "")
        in
        let code_body = compile_rec body_exp in
        let this_fun =
          spf "%s %s(%s)\n{\n%s\n%s\n}\n" (ty_to_string ty) x
            (String.concat ", " (code_tm_args @ code_hs))
            code_init code_body
        in
        fs := this_fun :: !fs;
        "CURRENTLY ONLY SUPPORT FUNCTIONS ASSIGNED TO A VARIABLE OR A HANDLER"
    | FullApply (((lhs_name, lhs_attrs) as lhs), es, hs, exps) ->
        let lhs' = compile_rec lhs in
        let exps' =
          List.map
            (fun ((exp, attrs) as rexp) ->
              let exp_code = compile_rec rexp in
              match attrs.ty with
              | TAbs _ -> spf "%s_fptr, %s_env" exp_code exp_code
              | _ -> exp_code)
            exps
        in
        let handler_args =
          List.map
            (fun x -> spf "locals.%s_fptr, locals.%s_env, locals.%s_jb" x x x)
            hs
        in
        let args_code =
          if attrs.isBuiltin then exps' @ handler_args
          else if attrs.isRecursiveCall then
            ("locals.env" :: exps') @ handler_args
          else ((lhs' ^ "_env") :: exps') @ handler_args
        in
        let lhs_code =
          if attrs.isBuiltin then
            let[@warning "-partial-match"] (Var x) = lhs_name in
            x
          else if Option.is_some attrs.topLevelFunctionName then
            Option.get attrs.topLevelFunctionName
          else (
            print_info "lhs_ty: %t@." (Print.ty lhs_attrs.ty);
            print_info "lhs_name: %t@." (Print.expr lhs_name);
            spf "((%s)%s_fptr)" (tabs_to_string lhs_attrs.ty false) lhs')
        in
        spf "%s(%s)" lhs_code (String.concat ", " args_code)
    | Raise (h, es, hs, exps) ->
        let exps' =
          List.map
            (fun ((exp, attrs) as rexp) ->
              let exp_code = compile_rec rexp in
              match attrs.ty with
              | TAbs _ -> spf "%s_fptr, %s_env" exp_code exp_code
              | _ -> exp_code)
            exps
        in
        let handler_code = compile_hvar h in
        let handler_args =
          List.map
            (fun x -> spf "locals.%s_fptr, locals.%s_env, locals.%s_jb" x x x)
            hs
        in
        let args_code =
          ((handler_code ^ "_env") :: (handler_code ^ "_jb") :: exps')
          @ handler_args
        in
        let lhs_code =
          if Option.is_some attrs.topLevelFunctionName then
            Option.get attrs.topLevelFunctionName
          else
            spf "((%s)%s_fptr)"
              (tabs_to_string (Option.get attrs.lhsHvar).ty true)
              handler_code
        in
        spf "%s(%s)" lhs_code (String.concat ", " args_code)
    | Resume e ->
        let[@warning "-partial-match"] FullApply (resumer, es, hs, [ ret ]), _ =
          e
        in
        let resumer' = compile_rec resumer in
        let ret' = compile_rec ret in
        spf "mp_resume(%s, (void*)%s);" resumer' ret'
    | Seq (e1, e2) ->
        let e1' = compile_rec e1 in
        let e2' = compile_rec e2 in
        spf "\n%s;\n%s;" e1' e2')
    |> fun code ->
    if can_be_returned exp then
      match attrs.cfDest with
      | Return ->
          (if attrs.isRecursiveCall then "__attribute__((musttail))" else "")
          ^ "return " ^ code ^ ";"
      | Abort -> spf "jmpret = %s;\n_longjmp(jb, 1);\n" code
      | Continue -> code
    else code
  in
  let _ = compile_rec exp in
  !global_code ^ String.concat "\n" (List.rev !fs)
