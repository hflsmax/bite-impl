open Syntax
open Common
open Util

let is_handle exp : bool = match exp with Handle _ -> true | _ -> false

let generalHandlerCodeGen ((exp, attrs) : expr) (codeGen : expr -> string) :
    string =
  let[@warning "-partial-match"] (Handle (hvar, fname, exp_catch, exp_handle)) =
    exp
  in
  let[@warning "-partial-match"] FullFun (fun_name, _, _, _, _, _, _), _ =
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
  print_info "%t@." (Print.expr (fst exp_handle));
  let _ = codeGen exp_catch in
  let _ = codeGen exp_handle in
  (if attrs.cfDest = Continue then "({" else "")
  ^ spf "locals.%s_fptr = (void*)%s;\n" hvar fun_name
  ^ spf "locals.%s_env = &locals;\n" hvar
  ^ spf "mp_prompt(%s, &locals);" wrapper_fun_name
  ^ if attrs.cfDest = Continue then ";})" else ""
