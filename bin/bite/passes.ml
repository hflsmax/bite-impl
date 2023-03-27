open Syntax
open Syntax.R
open Common
open Sexplib.Std
open Util
open Pass_util

let get_var_depth (x : name) (slink : static_link) : int =
  let rec get_var_depth' (x : name) (slink : static_link) (depth : int) : int =
    match slink with
    | [] ->
        error "Variable \"%s\" not found in static link: %t@." x
          (Print.static_link slink)
    | locals :: slink' ->
        if List.mem_assoc x locals then depth
        else get_var_depth' x slink' (depth + 1)
  in
  get_var_depth' x slink 0

let analyze_lambda_kind (f : R.expr') : lambda_kind =
  match f with
  | FullFun (GeneralHandler, _, _, _, _, _, _, body) -> (
      match body with Resume _, _, _, _ -> TailResumptive | _ -> Abortive)
  | FullFun (Lambda, _, _, _, _, _, _, body) -> Lambda
  | _ -> error "Handler is not a full function: %t@." (Print.rexpr' f)

let wrap_in_main (exp : expr) : expr =
  let fun_def = FullFun (Lambda, "main", [], [], [], TInt, [], exp) in
  (fun_def, full_fun_to_tabs fun_def, [], { default_attrs with cfDest = Return })

(*
    1. record variable depth
    2. record type of fnames
    3. analyze lambda kind
*)
let enrich_type (eff_defs : f_ENV) (exp : R.expr) : R.expr =
  let rec enrich_type' ((exp, ty, effs, attrs) : R.expr) (slink : static_link) :
      R.expr =
    ( (match exp with
      | Var (_, x) ->
          if attrs.isBuiltin then exp else Var (get_var_depth x slink, x)
      | Times (e1, e2) -> Times (enrich_type' e1 slink, enrich_type' e2 slink)
      | Plus (e1, e2) -> Plus (enrich_type' e1 slink, enrich_type' e2 slink)
      | Minus (e1, e2) -> Minus (enrich_type' e1 slink, enrich_type' e2 slink)
      | Equal (e1, e2) -> Equal (enrich_type' e1 slink, enrich_type' e2 slink)
      | Less (e1, e2) -> Less (enrich_type' e1 slink, enrich_type' e2 slink)
      | Assign (e1, e2) -> Assign (enrich_type' e1 slink, enrich_type' e2 slink)
      | Deref e -> Deref (enrich_type' e slink)
      | If (e1, e2, e3) ->
          If
            (enrich_type' e1 slink, enrich_type' e2 slink, enrich_type' e3 slink)
      | Let (x, ty, e1, e2) ->
          Let (x, ty, enrich_type' e1 slink, enrich_type' e2 slink)
      | Decl (x, ty, e1, e2) ->
          Decl (x, ty, enrich_type' e1 slink, enrich_type' e2 slink)
      | Handle (x, (fname, _), exp_catch, exp_handle) ->
          Handle
            ( x,
              (fname, List.assoc fname eff_defs),
              enrich_type' exp_catch slink,
              enrich_type' exp_handle slink )
      | FullFun (_, x, es1, hs, tm_args, ty, es2, exp_body) ->
          let hs' =
            List.map
              (fun (x, fname, _) -> (x, fname, List.assoc fname eff_defs))
              hs
          in
          let hd_args = List.map (fun (name, _, ty) -> (name, ty)) hs in
          let locals =
            ((x, full_fun_to_tabs exp) :: tm_args)
            @ hd_args @ gather_locals exp_body
          in
          FullFun
            ( analyze_lambda_kind exp,
              x,
              es1,
              hs',
              tm_args,
              ty,
              es2,
              enrich_type' exp_body (locals :: slink) )
      | FullApply (exp, es, hs, exps) ->
          FullApply
            ( enrich_type' exp slink,
              es,
              hs,
              List.map (fun exp_iter -> enrich_type' exp_iter slink) exps )
      | Raise (h, es, hs, exps) ->
          Raise
            ( h,
              es,
              hs,
              List.map (fun exp_iter -> enrich_type' exp_iter slink) exps )
      | Resume e -> Resume (enrich_type' e slink)
      | Seq (e1, e2) -> Seq (enrich_type' e1 slink, enrich_type' e2 slink)
      | (Int _ | Bool _) as e -> e),
      ty,
      effs,
      attrs )
  in
  enrich_type' exp (gather_locals exp :: [])

type pass_state = {
  curr_func_name : string;
  curr_func_is_tail_recursive : bool;
  value_store : (string * string) list;
}
[@@deriving sexp]

let check_tail_recursive state ((exp, ty, effs, attrs) : R.expr) =
  match exp with
  | FullFun (kind, x, es1, hs, tm_args, ty, es2, exp_body) ->
      (* If there is a recursive call that is not in tail position, set to false *)
      if
        List.length
          (gather_exp true
             (function
               | FullApply ((Var (_, x'), _, _, _), _, _, _), _, _, attrs ->
                   x = x' && not attrs.isRecursiveCall
               | _ -> false)
             exp_body)
        != 0
      then { state with curr_func_is_tail_recursive = false }
      else { state with curr_func_is_tail_recursive = true }
  | _ -> state

let mark_optimized_sjlj state ((exp, ty, effs, attrs) : R.expr) =
  match exp with
  | Handle (x, (fname, _), exp_catch, exp_handle) ->
      if state.curr_func_is_tail_recursive then
        (exp, ty, effs, { attrs with isOptimizedSjlj = true })
      else (exp, ty, effs, attrs)
  | _ -> (exp, ty, effs, attrs)

(* If tail-resumptive, remove resume expression such that it's treated like a function *)
let transform_handler _ ((exp, ty, effs, attrs) : R.expr) : R.expr =
  ( (match exp with
    | FullFun
        ( TailResumptive,
          x,
          es1,
          hs,
          tm_args,
          ty,
          es2,
          (exp_body, exp_body_ty, exp_body_es, exp_body_attrs) ) ->
        let[@warning "-partial-match"] (Resume exp_body') = exp_body in
        FullFun (TailResumptive, x, es1, hs, tm_args, ty, es2, exp_body')
    | FullFun
        ( Abortive,
          x,
          es1,
          hs,
          tm_args,
          ty,
          es2,
          (exp_body, exp_body_ty, exp_body_es, exp_body_attrs) ) ->
        exp
    | FullFun
        ( GeneralHandler,
          x,
          es1,
          hs,
          tm_args,
          ty,
          es2,
          (exp_body, exp_body_ty, exp_body_es, exp_body_attrs) ) ->
        error "Other handler kind not supported"
    | _ -> exp),
    ty,
    effs,
    attrs )

let mark_recursive_call state ((exp, ty, effs, attrs) : R.expr) : R.expr =
  match exp with
  | FullApply ((Var (_, x), _, _, _), _, _, _) ->
      if x = state.curr_func_name then
        (exp, ty, effs, { attrs with isRecursiveCall = true })
      else (exp, ty, effs, attrs)
  | _ -> (exp, ty, effs, attrs)

let propogate_const_fun_to_callsite state ((exp, ty, effs, attrs) : R.expr) :
    R.expr =
  match exp with
  | FullApply
      ((Var (_, x), x_ty, x_effs, x_attrs), app_eargs, app_hargs, app_targs)
    -> (
      match List.assoc_opt x state.value_store with
      | Some fun_name ->
          (exp, ty, effs, { attrs with topLevelFunctionName = Some fun_name })
      | None -> (exp, ty, effs, attrs))
  | Raise ((x, _, _), es, hs, exps) -> (
      match List.assoc_opt x state.value_store with
      | Some fun_name ->
          (exp, ty, effs, { attrs with topLevelFunctionName = Some fun_name })
      | None -> (exp, ty, effs, attrs))
  | _ -> (exp, ty, effs, attrs)

let mark_builtin_call _ ((exp, ty, effs, attrs) : R.expr) : R.expr =
  match exp with
  | FullApply ((_, _, _, x_attrs), _, _, _) ->
      if x_attrs.isBuiltin then (exp, ty, effs, { attrs with isBuiltin = true })
      else (exp, ty, effs, attrs)
  | _ -> (exp, ty, effs, attrs)

(* Mark children's cf_dest according to the current cf_dest *)
let mark_cf_dest _ ((exp, ty, effs, attrs) as rexp : R.expr) : R.expr =
  let mark cf_dest (exp, ty, effs, attrs) =
    (exp, ty, effs, { attrs with cfDest = cf_dest })
  in
  match exp with
  | Var _ | Int _ | Bool _ -> rexp
  | Times (e1, e2) ->
      (Times (mark Continue e1, mark Continue e2), ty, effs, attrs)
  | Plus (e1, e2) -> (Plus (mark Continue e1, mark Continue e2), ty, effs, attrs)
  | Minus (e1, e2) ->
      (Minus (mark Continue e1, mark Continue e2), ty, effs, attrs)
  | Equal (e1, e2) ->
      (Equal (mark Continue e1, mark Continue e2), ty, effs, attrs)
  | Less (e1, e2) -> (Less (mark Continue e1, mark Continue e2), ty, effs, attrs)
  | Deref e -> (Deref (mark Continue e), ty, effs, attrs)
  | Assign (e1, e2) ->
      (Assign (mark Continue e1, mark Continue e2), ty, effs, attrs)
  | If (e1, e2, e3) ->
      ( If (mark Continue e1, mark attrs.cfDest e2, mark attrs.cfDest e3),
        ty,
        effs,
        attrs )
  | Let (x, ty, e1, e2) ->
      (Let (x, ty, mark Continue e1, mark attrs.cfDest e2), ty, effs, attrs)
  | Decl (x, ty, e1, e2) ->
      (Decl (x, ty, mark Continue e1, mark attrs.cfDest e2), ty, effs, attrs)
  | Handle (x, fname, e1, e2) ->
      ( Handle (x, fname, mark Continue e1, mark attrs.cfDest e2),
        ty,
        effs,
        attrs )
  | FullFun (kind, x, es1, hs, tm_args, ty, es2, e) -> (
      match kind with
      | TailResumptive | Lambda ->
          ( FullFun (kind, x, es1, hs, tm_args, ty, es2, mark Return e),
            ty,
            effs,
            attrs )
      | Abortive ->
          ( FullFun (kind, x, es1, hs, tm_args, ty, es2, mark Abort e),
            ty,
            effs,
            attrs )
      | GeneralHandler -> error "General handlers not supported")
  | FullApply (e1, eargs, hargs, targs) ->
      ( FullApply
          (mark Continue e1, eargs, hargs, List.map (mark Continue) targs),
        ty,
        effs,
        attrs )
  | Raise (x, es, hs, exps) ->
      (Raise (x, es, hs, List.map (mark Continue) exps), ty, effs, attrs)
  | Resume e -> (Resume (mark attrs.cfDest e), ty, effs, attrs)
  | Seq (e1, e2) ->
      (Seq (mark Continue e1, mark attrs.cfDest e2), ty, effs, attrs)

(* Bottom-up AST walker *)
let transform_exp init_state exp_passes state_passes (exp : R.expr) : R.expr =
  let rec transform_exp_rec state ((exp, ty, effs, attrs) as rexp : R.expr) :
      R.expr =
    (* Add pre-transformer here. This corresponds to a top-dowm transformation *)
    let ((exp_pre, ty_pre, effs_pre, attrs_pre) as rexp_pre) =
      List.fold_left (fun rexp pass -> pass state rexp) rexp exp_passes
    in
    let state =
      List.fold_left (fun state pass -> pass state rexp) state state_passes
    in
    ( (match exp_pre with
      | Times (e1, e2) ->
          Times (transform_exp_rec state e1, transform_exp_rec state e2)
      | Plus (e1, e2) ->
          Plus (transform_exp_rec state e1, transform_exp_rec state e2)
      | Minus (e1, e2) ->
          Minus (transform_exp_rec state e1, transform_exp_rec state e2)
      | Equal (e1, e2) ->
          Equal (transform_exp_rec state e1, transform_exp_rec state e2)
      | Less (e1, e2) ->
          Less (transform_exp_rec state e1, transform_exp_rec state e2)
      | Assign (e1, e2) ->
          Assign (transform_exp_rec state e1, transform_exp_rec state e2)
      | Deref e -> Deref (transform_exp_rec state e)
      | If (e1, e2, e3) ->
          If
            ( transform_exp_rec state e1,
              transform_exp_rec state e2,
              transform_exp_rec state e3 )
      | Let (x, ty, e1, e2) ->
          let e1' = transform_exp_rec state e1 in
          let new_state_for_e2 =
            match e1 with
            | ( FullFun (kind, fun_name, es1, hs, tm_args, ty, es2, exp_body),
                _,
                _,
                _ ) ->
                { state with value_store = (x, fun_name) :: state.value_store }
            | _ -> state
          in
          let e2' = transform_exp_rec new_state_for_e2 e2 in
          Let (x, ty, e1', e2')
      | Decl (x, ty, e1, e2) ->
          Decl (x, ty, transform_exp_rec state e1, transform_exp_rec state e2)
      | Handle (x, h, exp_catch, exp_handle) ->
          let[@warning "-partial-match"] ( FullFun
                                             ( kind,
                                               fun_name,
                                               es1,
                                               hs,
                                               tm_args,
                                               ty,
                                               es2,
                                               exp_body ),
                                           _,
                                           _,
                                           _ ) =
            exp_catch
          in
          Handle
            ( x,
              h,
              transform_exp_rec state exp_catch,
              transform_exp_rec
                { state with value_store = (x, fun_name) :: state.value_store }
                exp_handle )
      | FullFun (kind, x, es1, hs, tm_args, ty, es2, exp_body) ->
          FullFun
            ( kind,
              x,
              es1,
              hs,
              tm_args,
              ty,
              es2,
              transform_exp_rec
                (* Remember the name of self so to identify recursive function. *)
                {
                  state with
                  curr_func_name = x;
                  value_store = (x, x) :: state.value_store;
                }
                exp_body )
      | FullApply (exp, es, hs, exps) ->
          FullApply
            ( transform_exp_rec state exp,
              es,
              hs,
              List.map (transform_exp_rec state) exps )
      | Raise (h, es, hs, exps) ->
          Raise (h, es, hs, List.map (transform_exp_rec state) exps)
      | Resume e -> Resume (transform_exp_rec state e)
      | Seq (e1, e2) ->
          Seq (transform_exp_rec state e1, transform_exp_rec state e2)
      | (Int _ | Bool _ | Var _) as e -> e),
      ty_pre,
      effs_pre,
      attrs_pre )
  in
  transform_exp_rec init_state exp

let init_state =
  { curr_func_name = ""; curr_func_is_tail_recursive = false; value_store = [] }

let transform (exp : R.expr) : R.expr =
  exp
  |> transform_exp init_state
       [
         transform_handler;
         mark_cf_dest;
         mark_recursive_call;
         propogate_const_fun_to_callsite;
         mark_builtin_call;
       ]
       []
  |> transform_exp init_state [ mark_optimized_sjlj ] [ check_tail_recursive ]
