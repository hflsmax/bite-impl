open Syntax
open Common

let is_handle exp : bool = match exp with Handle _ -> true | _ -> false

let generalHandlerCodeGen ((exp, attrs) : expr) (codeGen : expr -> string)
    (global_code : string ref) : string =
  let[@warning "-partial-match"] (Handle (hvar, fname, exp_catch, exp_handle)) =
    exp
  in
  let[@warning "-partial-match"] ( FullFun
                                     (fun_name, _, _, _, _, _, exp_catch_body),
                                   _ ) =
    exp_catch
  in
  let[@warning "-partial-match"] ( FullFun
                                     ( wrapper_fun_name,
                                       _,
                                       _,
                                       _,
                                       _,
                                       _,
                                       wrapper_fun_body ),
                                   _ ) =
    exp_handle
  in
  let _ = codeGen exp_catch in
  global_code :=
    !global_code
    ^ spf "%s %s(mp_prompt_t* p, void* env) {\n" (ty_to_string attrs.ty)
        wrapper_fun_name
    ^ spf "%s_locals_t locals;\n" wrapper_fun_name
    ^ spf "locals.env = (%s_env_t *)env;\n" fun_name
    ^ spf "locals.env->%s_jb = p;\n" hvar
    ^ codeGen wrapper_fun_body ^ "}\n";
  (if attrs.cfDest = Continue then "({" else "")
  ^ spf "locals.%s_fptr = (void*)%s;\n" hvar fun_name
  ^ spf "locals.%s_env = &locals;\n" hvar
  ^ spf "mp_prompt(%s, &locals);" wrapper_fun_name
  ^ if attrs.cfDest = Continue then ";})" else ""
