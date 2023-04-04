open Syntax
open Pass_state

[@@@ocaml.warning "-unused-open"]

open Util

let fn_index = ref 0

let transform_nameless_function _ ((exp, attrs) : expr) : expr =
  match exp with
  | Let
      ( x,
        isTop,
        (FullFun ("", es1, hs, tm_args, ty, es2, exp_body), fattrs),
        exp2 ) ->
      ( Let
          ( x,
            isTop,
            ( FullFun
                ( (fn_index := !fn_index + 1;
                   x ^ "_" ^ string_of_int !fn_index),
                  es1,
                  hs,
                  tm_args,
                  ty,
                  es2,
                  exp_body ),
              fattrs ),
            exp2 ),
        attrs )
  | Handle
      ( x,
        fname,
        (FullFun ("", es1, hs, tm_args, ty, es2, exp_body), fattrs),
        exp_handle ) ->
      ( Handle
          ( x,
            fname,
            ( FullFun
                ( (fn_index := !fn_index + 1;
                   x ^ "_" ^ string_of_int !fn_index),
                  es1,
                  hs,
                  tm_args,
                  ty,
                  es2,
                  exp_body ),
              fattrs ),
            exp_handle ),
        attrs )
  | _ -> (exp, attrs)

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

let chain_let l (e : expr) =
  List.fold_right
    (fun (x, isTop, e1, attrs) (e2 : expr) -> (Let (x, isTop, e1, e2), attrs))
    l e

let expand_hvar_and_funarg state ((exp, attrs) : expr) : expr =
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
      ( FullFun (x, [], [], expanded_tm_args @ expanded_hs, ty, [], exp_body),
        attrs )
  | FullApply (((lhs_expr, lhs_attrs) as lhs), es, hs, exps) ->
      let lhs_name =
        let[@warning "-partial-match"] (Var x) = lhs_expr in
        x
      in
      let expanded_tm_arg_ty_pairs =
        (if attrs.isBuiltin then []
        else if attrs.isRecursiveCall then
          [ ((Var "env", { default_attrs with ty = TBuiltin }), TBuiltin) ]
        else
          [
            ( (Var (lhs_name ^ "_env"), { default_attrs with ty = TBuiltin }),
              TBuiltin );
          ])
        :: (if attrs.isHandler then
            [ ((Var "jb", { default_attrs with ty = TBuiltin }), TBuiltin) ]
           else [])
        :: List.map
             (fun (exp, attrs) ->
               match attrs.ty with
               | TAbs _ ->
                   let[@warning "-partial-match"] (Var x) = exp in
                   [
                     ( (Var (x ^ "_fptr"), { default_attrs with ty = TBuiltin }),
                       TBuiltin );
                     ( (Var (x ^ "_env"), { default_attrs with ty = TBuiltin }),
                       TBuiltin );
                   ]
               | _ -> [ ((exp, attrs), attrs.ty) ])
             exps
        |> List.flatten
      in
      let expanded_hs_ty_pairs =
        List.map
          (fun h ->
            [
              ( (Var (h ^ "_fptr"), { default_attrs with ty = TBuiltin }),
                TBuiltin );
              ( (Var (h ^ "_env"), { default_attrs with ty = TBuiltin }),
                TBuiltin );
              ((Var (h ^ "_jb"), { default_attrs with ty = TBuiltin }), TBuiltin);
            ])
          hs
        |> List.flatten
      in
      let new_args, new_arg_tys =
        List.split (expanded_tm_arg_ty_pairs @ expanded_hs_ty_pairs)
      in
      let old_ret_type =
        let[@warning "-partial-match"] (TAbs (_, _, _, ret_ty, _)) =
          lhs_attrs.ty
        in
        ret_ty
      in
      let new_lhs_attrs =
        { lhs_attrs with ty = TAbs ([], [], new_arg_tys, old_ret_type, []) }
      in
      let new_lhs =
        if attrs.isBuiltin || List.mem lhs_name state.func_names then lhs
        else (Var (lhs_name ^ "_fptr"), new_lhs_attrs)
      in
      (FullApply (new_lhs, [], [], new_args), attrs)
  | Raise (h, es, hs, exps) ->
      let expanded_tm_arg_ty_pairs =
        [
          ((Var (h ^ "_env"), { default_attrs with ty = TBuiltin }), TBuiltin);
          ((Var (h ^ "_jb"), { default_attrs with ty = TBuiltin }), TBuiltin);
        ]
        :: List.map
             (fun (exp, attrs) ->
               match attrs.ty with
               | TAbs _ ->
                   let[@warning "-partial-match"] (Var x) = exp in
                   [
                     ( (Var (x ^ "_fptr"), { default_attrs with ty = TBuiltin }),
                       TBuiltin );
                     ( (Var (x ^ "_env"), { default_attrs with ty = TBuiltin }),
                       TBuiltin );
                   ]
               | _ -> [ ((exp, attrs), attrs.ty) ])
             exps
        |> List.flatten
      in
      let expanded_hs_ty_pairs =
        List.map
          (fun h ->
            [
              ( (Var (h ^ "_fptr"), { default_attrs with ty = TBuiltin }),
                TBuiltin );
              ( (Var (h ^ "_env"), { default_attrs with ty = TBuiltin }),
                TBuiltin );
              ((Var (h ^ "_jb"), { default_attrs with ty = TBuiltin }), TBuiltin);
            ])
          hs
        |> List.flatten
      in
      let new_args, new_arg_tys =
        List.split (expanded_tm_arg_ty_pairs @ expanded_hs_ty_pairs)
      in
      let old_ret_type =
        let[@warning "-partial-match"] (TAbs (_, _, _, ret_ty, _)) =
          (Option.get attrs.lhsHvar).ty
        in
        ret_ty
      in
      let new_lhs_attrs =
        { default_attrs with ty = TAbs ([], [], new_arg_tys, old_ret_type, []) }
      in
      (FullApply ((Var (h ^ "_fptr"), new_lhs_attrs), [], [], new_args), attrs)
  | Let (x, isTop, ((FullFun (fun_name, _, _, _, _, _, _), fattrs) as e1), e2)
    ->
      chain_let
        [
          (x ^ "_fptr", false, e1, attrs);
          (* TODO: replace false with isTop. The reason why is false is that we want the codeGen to codeGen for e1. *)
          ( x ^ "_env",
            isTop,
            (Aux ReifyEnvironment, { default_attrs with ty = TBuiltin }),
            attrs );
        ]
        e2
  | Handle (x, _, catch_exp, handle_exp) ->
      chain_let
        [
          (x ^ "_fptr", false, catch_exp, attrs);
          ( x ^ "_env",
            false,
            (Aux ReifyEnvironment, { default_attrs with ty = TBuiltin }),
            attrs );
          ( x ^ "_jb",
            false,
            ( Aux
                (match[@warning "-partial-match"] attrs.handlerKind with
                | Some Abortive -> ReifyFixedContext
                | Some Multishot | Some SingleShot -> ReifyContextIndirection
                | Some TailResumptive -> Noop),
              { default_attrs with ty = TBuiltin } ),
            { attrs with skipDef = attrs.handlerKind = Some TailResumptive } );
        ]
        handle_exp
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

let transform_topCall state ((exp, attrs) : expr) : expr =
  match exp with
  | FullApply ((Var x, x_attrs), app_eargs, app_hargs, app_targs) -> (
      match List.assoc_opt x state.value_store with
      | Some fun_name ->
          ( FullApply ((Var fun_name, x_attrs), app_eargs, app_hargs, app_targs),
            { attrs with isTopCall = true } )
      | None -> (exp, attrs))
  | Raise (x, es, hs, exps) -> (
      match List.assoc_opt x state.value_store with
      | Some fun_name ->
          (Raise (fun_name, es, hs, exps), { attrs with isTopCall = true })
      | None -> (exp, attrs))
  | _ -> (exp, attrs)

let transform_reify_context state ((exp, attrs) : expr) : expr =
  match exp with
  | Let (x, isTop, (Aux ReifyFixedContext, aattrs), e2) ->
      if state.curr_func_is_tail_recursive then
        let new_e2 =
          ( If
              ( mk_bop "||"
                  (mk_var (x ^ "_saved"))
                  (mk_uop "!" (mk_apply_1 (mk_builtin_fun "setjmp") (mk_var x))),
                mk_seq (mk_asgn (mk_var (x ^ "_saved")) (mk_bool true)) e2,
                ( Var "jmpret",
                  { default_attrs with ty = TInt; isBuiltin = true } ) ),
            snd e2 )
        in
        chain_let
          [
            ( x,
              true,
              ( Aux ReifyFixedContext,
                { default_attrs with ty = TCustom "jmp_buf" } ),
              { attrs with skipDef = true } );
            ( x ^ "_saved",
              true,
              mk_int 0,
              { default_attrs with ty = TBool; skipDef = true } );
          ]
          new_e2
      else
        let new_e2 =
          ( If
              ( ( FullApply
                    ( mk_builtin_fun "setjmp",
                      [],
                      [],
                      [ (Var x, { default_attrs with ty = TBuiltin }) ] ),
                  { default_attrs with ty = TBool } ),
                e2,
                ( Var "jmpret",
                  { default_attrs with ty = TInt; isBuiltin = true } ) ),
            snd e2 )
        in
        ( Let
            ( x,
              isTop,
              (Aux ReifyFixedContext, { aattrs with ty = TCustom "jmp_buf*" }),
              new_e2 ),
          attrs )
  | _ -> (exp, attrs)

let transform_handler _ ((exp, attrs) : expr) : expr =
  match exp with
  | FullFun
      (x, es, hs, tm_args, ty, es2, ((exp_body, exp_body_attrs) as rexp_body))
    ->
      if attrs.handlerKind = Some TailResumptive then
        let[@warning "-partial-match"] (Resume (exp_body', r)) = exp_body in
        (FullFun (x, es, hs, tm_args, ty, es2, exp_body'), attrs)
      else if attrs.handlerKind = Some Abortive then
        let new_body =
          ( Let
              ( "jmpret",
                true,
                rexp_body,
                ( FullApply
                    (mk_builtin_fun "longjmp", [], [], [ mk_var "jb"; mk_int 1 ]),
                  exp_body_attrs ) ),
            { exp_body_attrs with isBuiltin = true } )
        in
        (FullFun (x, es, hs, tm_args, ty, es2, new_body), attrs)
      else (exp, attrs)
  | _ -> (exp, attrs)

(*
   ( (match exp with
     | FullFun (x, es1, hs, tm_args, ty, es2, (exp_body, exp_body_attrs)) -> (
         match exp_body with
         | Resume (exp_body', r) ->
             FullFun (x, es1, hs, tm_args, ty, es2, exp_body')
         | _ -> exp)
     | _ -> exp),
     attrs ) *)
