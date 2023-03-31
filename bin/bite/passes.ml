open Syntax
open Common
open Sexplib.Std
open Util
open Pass_util

let mark_handlers state ((exp, attrs) : expr) =
  match exp with
  | Handle (x, fname, (c, cattrs), exp_handle) ->
      ( Handle (x, fname, (c, { cattrs with isHandler = true }), exp_handle),
        attrs )
  | _ -> (exp, attrs)

let get_var_depth (x : name) (slink : string list list) : int =
  let rec get_var_depth' (x : name) (slink : string list list) (depth : int) :
      int =
    match slink with
    | [] -> error "Variable \"%s\" not found in static link@." x
    | locals :: slink' ->
        if List.mem x locals then depth else get_var_depth' x slink' (depth + 1)
  in
  get_var_depth' x slink 0

let analyze_handlerKind (f : expr') : handlerKind =
  match f with
  | FullFun (_, _, _, _, _, _, body) -> (
      match body with
      | Resume _, _ -> TailResumptive
      | _ ->
          if
            List.length
              (gather_exp true
                 (fun exp -> match exp with Resume _, _ -> true | _ -> false)
                 body)
            > 0
          then Multishot
          else Abortive)
  | _ -> error "Handler is not a full function: %t@." (Print.expr f)

let wrap_in_main (exp : expr) : expr =
  let fun_def = FullFun ("main", [], [], [], TInt, [], exp) in
  ( fun_def,
    {
      default_attrs with
      cfDest = Return;
      ty = full_fun_to_tabs fun_def;
      effs = [];
    } )

type pass_state = {
  curr_func_name : string;
  curr_func_is_tail_recursive : bool;
  value_store : (string * string) list;
  static_link : string list list;
  eff_defs : f_ENV;
}
[@@deriving sexp]

let wrap_general_handler_body _ ((exp, attrs) : expr) : expr =
  if attrs.handlerKind = Some Multishot || attrs.handlerKind = Some SingleShot
  then
    match exp with
    | Handle
        ( x,
          fname,
          (FullFun (fun_name, es1, hs, tm_args, ty, es2, exp_body), fattrs),
          exp_handle ) ->
        let wrapper =
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
            ( x,
              fname,
              (FullFun (fun_name, es1, hs, tm_args, ty, es2, exp_body), fattrs),
              (wrapper, default_attrs) ),
          attrs )
    | _ -> (exp, attrs)
  else (exp, attrs)

let record_handlerKind state ((exp, attrs) : expr) =
  if attrs.handlerKind <> None then (exp, attrs)
  else
    match exp with
    | Handle (x, fname, (f, fattrs), exp_handle) ->
        ( Handle
            ( x,
              fname,
              (f, { fattrs with handlerKind = Some (analyze_handlerKind f) }),
              exp_handle ),
          { attrs with handlerKind = Some (analyze_handlerKind f) } )
    | _ -> (exp, attrs)

let update_static_link state ((exp, attrs) : expr) =
  match exp with
  | FullFun (x, es1, hs, tm_args, ty, es2, exp_body) ->
      let locals = (x :: List.map fst tm_args) @ List.map fst hs in
      { state with static_link = locals :: state.static_link }
  | Let (x, e1, e2) ->
      {
        state with
        static_link =
          (x :: List.hd state.static_link) :: List.tl state.static_link;
      }
  | Decl (x, e1, e2) ->
      {
        state with
        static_link =
          (x :: List.hd state.static_link) :: List.tl state.static_link;
      }
  | Handle (x, fname, exp_catch, exp_handle) ->
      {
        state with
        static_link =
          (x :: List.hd state.static_link) :: List.tl state.static_link;
      }
  | _ -> state

let record_var_depth state ((exp, attrs) : expr) =
  match exp with
  | Var x ->
      if attrs.isBuiltin then (exp, attrs)
      else
        let depth = get_var_depth x state.static_link in
        (exp, { attrs with varDepth = depth })
  | Raise (x, _, hvars, _) ->
      let hvarArgs' =
        List.map
          (fun hvar ->
            let depth = get_var_depth hvar.name state.static_link in
            { hvar with depth })
          attrs.hvarArgs
      in
      let depth = get_var_depth x state.static_link in
      ( exp,
        {
          attrs with
          lhsHvar = Some { (Option.get attrs.lhsHvar) with depth };
          hvarArgs = hvarArgs';
        } )
  | FullApply (_, _, hvars, _) ->
      let hvarArgs' =
        List.map
          (fun hvar ->
            let depth = get_var_depth hvar.name state.static_link in
            { hvar with depth })
          attrs.hvarArgs
      in
      (exp, { attrs with hvarArgs = hvarArgs' })
  | _ -> (exp, attrs)

let check_tail_recursive state ((exp, attrs) : expr) =
  match exp with
  | FullFun (x, es1, hs, tm_args, ty, es2, exp_body) ->
      (* If there is a recursive call that is not in tail position, set to false *)
      if
        List.length
          (gather_exp true
             (function
               | FullApply ((Var x', _), _, _, _), attrs ->
                   x = x' && not attrs.isRecursiveCall
               | _ -> false)
             exp_body)
        != 0
      then { state with curr_func_is_tail_recursive = false }
      else { state with curr_func_is_tail_recursive = true }
  | _ -> state

let mark_optimized_sjlj state ((exp, attrs) : expr) =
  match exp with
  | Handle (x, fname, exp_catch, exp_handle) ->
      if state.curr_func_is_tail_recursive then
        (exp, { attrs with isOptimizedSjlj = true })
      else (exp, attrs)
  | _ -> (exp, attrs)

(* If tail-resumptive, remove resume expression such that it's treated like a function *)
let transform_tail_resumptive_handler _ ((exp, attrs) : expr) : expr =
  ( (match exp with
    | FullFun (x, es1, hs, tm_args, ty, es2, (exp_body, exp_body_attrs)) -> (
        match exp_body with
        | Resume exp_body' -> FullFun (x, es1, hs, tm_args, ty, es2, exp_body')
        | _ -> exp)
    | _ -> exp),
    attrs )

let mark_recursive_call state ((exp, attrs) : expr) : expr =
  match exp with
  | FullApply ((Var x, _), _, _, _) ->
      if x = state.curr_func_name then
        (exp, { attrs with isRecursiveCall = true })
      else (exp, attrs)
  | _ -> (exp, attrs)

let propogate_const_fun_to_callsite state ((exp, attrs) : expr) : expr =
  match exp with
  | FullApply ((Var x, x_attrs), app_eargs, app_hargs, app_targs) -> (
      match List.assoc_opt x state.value_store with
      | Some fun_name ->
          (exp, { attrs with topLevelFunctionName = Some fun_name })
      | None -> (exp, attrs))
  | Raise (x, es, hs, exps) -> (
      match List.assoc_opt x state.value_store with
      | Some fun_name ->
          (exp, { attrs with topLevelFunctionName = Some fun_name })
      | None -> (exp, attrs))
  | _ -> (exp, attrs)

let mark_builtin_call _ ((exp, attrs) : expr) : expr =
  match exp with
  | FullApply ((_, x_attrs), _, _, _) ->
      if x_attrs.isBuiltin then (exp, { attrs with isBuiltin = true })
      else (exp, attrs)
  | _ -> (exp, attrs)

(* Mark children's cf_dest according to the current cf_dest *)
let mark_cf_dest _ ((exp, attrs) as rexp : expr) : expr =
  let mark cf_dest (exp, attrs) = (exp, { attrs with cfDest = cf_dest }) in
  match exp with
  | Var _ | Int _ | Bool _ -> rexp
  | Times (e1, e2) -> (Times (mark Continue e1, mark Continue e2), attrs)
  | Plus (e1, e2) -> (Plus (mark Continue e1, mark Continue e2), attrs)
  | Minus (e1, e2) -> (Minus (mark Continue e1, mark Continue e2), attrs)
  | Equal (e1, e2) -> (Equal (mark Continue e1, mark Continue e2), attrs)
  | Less (e1, e2) -> (Less (mark Continue e1, mark Continue e2), attrs)
  | Deref e -> (Deref (mark Continue e), attrs)
  | Assign (e1, e2) -> (Assign (mark Continue e1, mark Continue e2), attrs)
  | If (e1, e2, e3) ->
      (If (mark Continue e1, mark attrs.cfDest e2, mark attrs.cfDest e3), attrs)
  | Let (x, e1, e2) -> (Let (x, mark Continue e1, mark attrs.cfDest e2), attrs)
  | Decl (x, e1, e2) -> (Decl (x, mark Continue e1, mark attrs.cfDest e2), attrs)
  | Handle (x, fname, e1, e2) ->
      (Handle (x, fname, mark Continue e1, mark attrs.cfDest e2), attrs)
  | FullFun (x, es1, hs, tm_args, ty, es2, e) ->
      if attrs.handlerKind = Some Abortive then
        (FullFun (x, es1, hs, tm_args, ty, es2, mark Abort e), attrs)
      else (FullFun (x, es1, hs, tm_args, ty, es2, mark Return e), attrs)
  | FullApply (e1, eargs, hargs, targs) ->
      ( FullApply
          (mark Continue e1, eargs, hargs, List.map (mark Continue) targs),
        attrs )
  | Raise (x, es, hs, exps) ->
      (Raise (x, es, hs, List.map (mark Continue) exps), attrs)
  | Resume e -> (Resume (mark attrs.cfDest e), attrs)
  | Seq (e1, e2) -> (Seq (mark Continue e1, mark attrs.cfDest e2), attrs)

(* Bottom-up AST walker *)
let transform_exp init_state exp_passes state_passes (exp : expr) : expr =
  let rec transform_exp_rec state ((exp, attrs) as rexp : expr) : expr =
    (* Add pre-transformer here. This corresponds to a top-dowm transformation *)
    let ((exp_pre, attrs_pre) as rexp_pre) =
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
      | Let (x, e1, e2) ->
          let e1' = transform_exp_rec state e1 in
          let new_state_for_e2 =
            match e1 with
            | FullFun (fun_name, es1, hs, tm_args, ty, es2, exp_body), _ ->
                { state with value_store = (x, fun_name) :: state.value_store }
            | _ -> state
          in
          let e2' = transform_exp_rec new_state_for_e2 e2 in
          Let (x, e1', e2')
      | Decl (x, e1, e2) ->
          Decl (x, transform_exp_rec state e1, transform_exp_rec state e2)
      | Handle (x, h, exp_catch, exp_handle) ->
          let[@warning "-partial-match"] ( FullFun
                                             ( fun_name,
                                               es1,
                                               hs,
                                               tm_args,
                                               ty,
                                               es2,
                                               exp_body ),
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
      | FullFun (x, es1, hs, tm_args, ty, es2, exp_body) ->
          FullFun
            ( x,
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
      attrs_pre )
  in
  transform_exp_rec init_state exp

let transform (effs_efs : f_ENV) (exp : expr) : expr =
  let init_state =
    {
      curr_func_name = "";
      curr_func_is_tail_recursive = false;
      value_store = [];
      static_link = [];
      eff_defs = effs_efs;
    }
  in
  exp
  |> transform_exp init_state
       [
         mark_handlers;
         record_handlerKind;
         wrap_general_handler_body;
         record_var_depth;
         transform_tail_resumptive_handler;
         mark_cf_dest;
         mark_recursive_call;
         propogate_const_fun_to_callsite;
         mark_builtin_call;
       ]
       [ update_static_link ]
  |> transform_exp init_state [ mark_optimized_sjlj ] [ check_tail_recursive ]
