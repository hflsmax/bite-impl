(** Bite codeGenr. *)

open Syntax
open Common

[@@@ocaml.warning "-unused-open"]

open Util

let codeGen_var (v : string) (depth : int) =
  if depth = -1 then v
  else
    spf "locals.%s%s" (String.concat "" (List.init depth (fun _ -> "env->"))) v

let can_be_returned exp =
  match exp with
  | Var _ | Int _ | Bool _ | Unit | AOP _ | BOP _ | UOP _ | Deref _
  | FullApply _ | Raise _ ->
      true
  | Assign _ | If _ | Let _ | Decl _ | Handle _ | FullFun _ | Resume _ | Seq _
  | Aux _ ->
      false

(* Compile an expression to a top-level and a list of functions *)
let codeGen exp : string =
  let fun_decls = ref "" in
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
      | Aux auxF -> (
          match auxF with
          | ReifyEnvironment -> "&locals"
          | ReifyFixedContext -> "({ jmp_buf tmp_buf; &tmp_buf; });"
          | Noop -> "Noop"
          | _ -> failwith "Aux not implemented")
      | Unit -> ""
      | Var x ->
          if attrs.isBuiltin then x
          else codeGen_var x (Option.get attrs.varDepth)
      | Int k -> string_of_int k
      | Bool b -> string_of_bool b
      | AOP (op, e1, e2) ->
          let e1' = codeGen_rec e1 in
          let e2' = codeGen_rec e2 in
          spf "({%s %s %s;})" e1' op e2'
      | BOP (op, e1, e2) ->
          let e1' = codeGen_rec e1 in
          let e2' = codeGen_rec e2 in
          spf "({%s %s %s;})" e1' op e2'
      | UOP (op, e) ->
          let e' = codeGen_rec e in
          spf "({%s %s;})" op e'
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
          if attrs.cfDest = Continue then
            spf "(%s ? ({%s;}) : ({%s;}) )" e1' e2' e3'
          else spf "if (%s) {\n%s;\n} else {\n%s;\n}" e1' e2' e3'
      | Let (x, isTop, e1, e2) -> (
          let e1' = codeGen_rec e1 in
          let e2' = codeGen_rec e2 ^ ";" in
          if isTop then (
            global_code :=
              (match e1 with
              | FullFun (fun_name, _, _, _, _, _, _), _ -> spf "void* %s;\n" x
              | _ ->
                  if attrs.isBuiltin then ""
                  else spf "volatile %s %s;\n" (ty_to_string (snd e1).ty) x)
              ^ (if attrs.defAtTop then spf "%s = %s;\n" x e1' else "")
              ^ !global_code;
            (if attrs.skipDef || attrs.defAtTop then ""
            else spf "%s = %s;\n" x e1')
            ^ e2')
          else
            match e1 with
            | FullFun (fun_name, _, _, _, _, _, _), fattrs ->
                (if attrs.skipDef then ""
                else spf "locals.%s = (void*)%s;\n" x fun_name)
                ^ e2' ^ "\n"
            | _ ->
                (if attrs.skipDef then "" else spf "locals.%s = %s;\n" x e1')
                ^ e2')
      | Decl (x, isTop, e1, e2) ->
          let e1' = codeGen_rec e1 in
          let e2' = codeGen_rec e2 in
          if isTop then (
            global_code :=
              spf "%s %s = %s;\n" (ty_to_string attrs.ty) x e1' ^ !global_code;
            e2')
          else spf "locals.%s = %s;\n%s" x e1' e2'
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
          let fun_decl =
            spf "%s %s(%s)\n" (ty_to_string ty) x
              (String.concat ", " code_tm_args)
          in
          let this_fun = spf "%s{\n%s\n%s\n}\n" fun_decl code_init code_body in
          fun_decls := !fun_decls ^ fun_decl ^ ";\n";
          global_code := !global_code ^ "\n" ^ this_fun ^ "\n";
          "CURRENTLY ONLY SUPPORT FUNCTIONS ASSIGNED TO A VARIABLE OR A HANDLER"
      | FullApply (((lhs_name, lhs_attrs) as lhs), es, hs, exps) ->
          let lhs' = codeGen_rec lhs in
          let args_code =
            List.map
              (fun ((exp, attrs) as rexp) ->
                let exp_code = codeGen_rec rexp in
                match attrs.ty with
                | TAbs _ -> spf "%s_fptr, %s_env" exp_code exp_code
                | _ -> exp_code)
              exps
          in
          let lhs_code =
            if attrs.isTopCall || attrs.isBuiltin then lhs'
            else spf "((%s)%s)" (tabs_to_string lhs_attrs.ty false) lhs'
          in
          spf "%s(%s)" lhs_code (String.concat ", " args_code)
      | Raise (h, es, hs, exps) -> "ALL RAISE SHOULD HAVE BECOME FULLAPPLY"
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
        | Continue -> code
      else code
  in
  let _ = codeGen_rec exp in
  !fun_decls ^ !global_code
