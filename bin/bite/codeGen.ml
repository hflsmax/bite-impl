(** Bite codeGenr. *)

open Syntax
open Common
open Util

let codeGen_hvar (hvar : richHvar) =
  spf "locals.%s%s"
    (String.concat "" (List.init hvar.depth (fun _ -> "env->")))
    hvar.name

let can_be_returned exp =
  match exp with
  | Var _ | Int _ | Bool _ | Unit | Times _ | Plus _ | Minus _ | Equal _
  | Less _ | Deref _ | FullApply _ | Raise _ ->
      true
  | Assign _ | If _ | Let _ | Decl _ | Handle _ | FullFun _ | Resume _ | Seq _
    ->
      false

(* Compile an expression to a top-level and a list of functions *)
let codeGen exp : string =
  let global_code = ref "" in
  let rec codeGen_rec ((exp, attrs) : expr) : string =
    if
      LibmpromptCodeGen.is_handle exp
      && (attrs.handlerKind = Some Multishot
         || attrs.handlerKind = Some SingleShot)
    then
      LibmpromptCodeGen.generalHandlerCodeGen (exp, attrs) codeGen_rec
        global_code
    else
      (match exp with
      | Unit -> ""
      | Var x ->
          if attrs.isBuiltin then x
          else
            spf "locals.%s%s"
              (String.concat "" (List.init attrs.varDepth (fun _ -> "env->")))
              x
      | Int k -> string_of_int k
      | Bool b -> string_of_bool b
      | Times (e1, e2) ->
          let e1' = codeGen_rec e1 in
          let e2' = codeGen_rec e2 in
          spf "({%s * %s;})" e1' e2'
      | Plus (e1, e2) ->
          let e1' = codeGen_rec e1 in
          let e2' = codeGen_rec e2 in
          spf "({%s + %s;})" e1' e2'
      | Minus (e1, e2) ->
          let e1' = codeGen_rec e1 in
          let e2' = codeGen_rec e2 in
          spf "({%s - %s;})" e1' e2'
      | Equal (e1, e2) ->
          let e1' = codeGen_rec e1 in
          let e2' = codeGen_rec e2 in
          spf "({%s == %s;})" e1' e2'
      | Less (e1, e2) ->
          let e1' = codeGen_rec e1 in
          let e2' = codeGen_rec e2 in
          spf "({%s < %s;})" e1' e2'
      | Deref e ->
          let e' = codeGen_rec e in
          e'
      | Assign (e1, e2) ->
          let e1' = codeGen_rec e1 in
          let e2' = codeGen_rec e2 in
          spf "%s = %s;\n" e1' e2'
      | If (e1, e2, e3) ->
          let e1' = codeGen_rec e1 in
          let e2' = codeGen_rec e2 in
          let e3' = codeGen_rec e3 in
          if attrs.cfDest = Continue then spf "(%s ? %s : %s)" e1' e2' e3'
          else spf "if (%s) {\n%s;\n} else {\n%s;\n}" e1' e2' e3'
      | Let (x, e1, e2) -> (
          let e1' = codeGen_rec e1 in
          let e2' = codeGen_rec e2 in
          match e1 with
          | FullFun (fun_name, _, _, _, _, _, _), fattrs ->
              spf "locals.%s_fptr = (void*)%s;\n" x fun_name
              ^ (if List.length fattrs.freeVars > 0 then
                 spf "locals.%s_env = &locals;\n" x
                else "")
              ^ e2' ^ "\n"
          | _ -> spf "locals.%s = %s;\n%s;" x e1' e2')
      | Decl (x, e1, e2) ->
          let e1' = codeGen_rec e1 in
          let e2' = codeGen_rec e2 in
          spf "locals.%s = %s;\n%s" x e1' e2'
      | Handle (handler_var_name, fname, exp_catch, exp_handle) ->
          let[@warning "-partial-match"] ( FullFun (fun_name, _, _, _, _, _, _),
                                           fattrs ) =
            exp_catch
          in
          let _ = codeGen_rec exp_catch in
          let exp_handle_code = codeGen_rec exp_handle in
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
                  if attrs.isOptimizedSjlj then
                    spf "%s_saved = true;\n" fun_name
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
          ^ (if List.length fattrs.freeVars > 0 then
             spf "locals.%s_env = &locals;\n" handler_var_name
            else "")
          ^ exp_handle_code
          ^ if attrs.cfDest = Continue then ";})" else ""
      | FullFun (x, es1, hs, tm_args, ty, es2, body_exp) ->
          let code_tm_args =
            List.map
              (fun (arg_name, arg_ty) -> ty_to_string arg_ty ^ " " ^ arg_name)
              tm_args
          in
          let code_init =
            spf "%s_locals_t locals;\n" x
            ^ (tm_args
              |> List.map (fun (arg_name, arg_ty) ->
                     spf "locals.%s = %s;\n" arg_name arg_name)
              |> String.concat "")
          in
          let code_body = codeGen_rec body_exp in
          let this_fun =
            spf "%s %s(%s)\n{\n%s\n%s\n}\n" (ty_to_string ty) x
              (String.concat ", " code_tm_args)
              code_init code_body
          in
          global_code := !global_code ^ "\n" ^ this_fun ^ "\n";
          "CURRENTLY ONLY SUPPORT FUNCTIONS ASSIGNED TO A VARIABLE OR A HANDLER"
      | FullApply (((lhs_name, lhs_attrs) as lhs), es, hs, exps) ->
          let lhs' = codeGen_rec lhs in
          let exps' =
            List.map
              (fun ((exp, attrs) as rexp) ->
                let exp_code = codeGen_rec rexp in
                match attrs.ty with
                | TAbs _ -> spf "%s_fptr, %s_env" exp_code exp_code
                | _ -> exp_code)
              exps
          in
          let handler_args =
            List.map
              (fun h ->
                h |> codeGen_hvar |> fun x -> spf "%s_fptr, %s_env, %s_jb" x x x)
              attrs.hvarArgs
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
                let exp_code = codeGen_rec rexp in
                match attrs.ty with
                | TAbs _ -> spf "%s_fptr, %s_env" exp_code exp_code
                | _ -> exp_code)
              exps
          in
          let handler_code = codeGen_hvar (Option.get attrs.lhsHvar) in
          let handler_args =
            List.map
              (fun h ->
                h |> codeGen_hvar |> fun x -> spf "%s_fptr, %s_env, %s_jb" x x x)
              attrs.hvarArgs
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
      | Resume (e, r) ->
          let[@warning "-partial-match"] (Some r) = r in
          let r' = codeGen_rec r in
          let ret' = codeGen_rec e in
          spf "mp_resume(%s, (void*)%s);" r' ret'
      | Seq (e1, e2) ->
          let e1' = codeGen_rec e1 in
          let e2' = codeGen_rec e2 in
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
  let _ = codeGen_rec exp in
  !global_code
