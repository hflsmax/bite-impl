open Syntax

(* This wrapper allows the body to be a function and takes in mp_prompt_t as the first argument *)
let transform_general_handler _ ((exp, attrs) : expr) : expr =
  if attrs.handlerKind = Some Multishot || attrs.handlerKind = Some SingleShot
  then
    match exp with
    | Handle
        ( x,
          fname,
          (FullFun (fun_name, es1, hs, tm_args, ty, es2, exp_body), fattrs),
          exp_handle ) ->
        let new_handler =
          FullFun (fun_name, [], [], [ ("r", TBuiltin) ], ty, [], exp_body)
        in
        let handler_wrapper =
          FullFun
            ( fun_name ^ "_handler_wrapper",
              es1,
              hs,
              tm_args,
              ty,
              es2,
              ( Let
                  ( fun_name,
                    false,
                    (new_handler, fattrs),
                    (FullApply ((Var fun_name, fattrs), [], [], []), fattrs) ),
                fattrs ) )
        in
        let body_wrapper =
          FullFun
            ( fun_name ^ "_body_wrapper",
              [],
              [],
              [],
              attrs.ty,
              attrs.effs,
              exp_handle )
        in
        ( Handle
            (x, fname, (handler_wrapper, fattrs), (body_wrapper, default_attrs)),
          attrs )
    | _ -> (exp, attrs)
  else (exp, attrs)

(* If tail-resumptive, remove resume expression such that it's treated like a function *)
let transform_tail_resumptive_handler _ ((exp, attrs) : expr) : expr =
  ( (match exp with
    | FullFun (x, es1, hs, tm_args, ty, es2, (exp_body, exp_body_attrs)) -> (
        match exp_body with
        | Resume (exp_body', r) ->
            FullFun (x, es1, hs, tm_args, ty, es2, exp_body')
        | _ -> exp)
    | _ -> exp),
    attrs )

let expand_hvar_and_funarg _ ((exp, attrs) : expr) : expr =
  match exp with
  | FullFun (x, es1, hs, tm_args, ty, es2, exp_body) ->
      let expanded_tm_args =
        List.map
          (function
            | arg_name, TAbs _ ->
                [
                  (arg_name ^ "_fptr", TBuiltin); (arg_name ^ "_env", TBuiltin);
                ]
            | p -> [ p ])
          tm_args
        |> List.flatten
      in
      let expanded_hs =
        List.map
          (fun (h, fname) ->
            [
              (h ^ "_fptr", TBuiltin);
              (h ^ "_env", TBuiltin);
              (h ^ "_jb", TBuiltin);
            ])
          hs
        |> List.flatten
      in
      ( FullFun (x, es1, [], expanded_tm_args @ expanded_hs, ty, es2, exp_body),
        attrs )
  | _ -> (exp, attrs)

let add_jb_arg_for_handler _ ((exp, attrs) : expr) : expr =
  match exp with
  | FullFun (x, es1, hs, tm_args, ty, es2, exp_body) ->
      if attrs.isHandler then
        ( FullFun (x, es1, hs, ("jb", TBuiltin) :: tm_args, ty, es2, exp_body),
          attrs )
      else (exp, attrs)
  | _ -> (exp, attrs)

let add_env_arg_for_fun _ ((exp, attrs) : expr) : expr =
  match exp with
  | FullFun (x, es1, hs, tm_args, ty, es2, exp_body) ->
      if x <> "main" then
        ( FullFun
            ( x,
              es1,
              hs,
              ("env", TCustom (x ^ "_env_t*")) :: tm_args,
              ty,
              es2,
              exp_body ),
          attrs )
      else (exp, attrs)
  | _ -> (exp, attrs)
