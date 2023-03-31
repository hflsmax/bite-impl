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
