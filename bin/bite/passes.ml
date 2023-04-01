open Syntax
open Common
open Pass_state
open Pass_attrs
open Pass_expr

let wrap_in_main (exp : expr) : expr =
  let fun_def = FullFun ("main", [], [], [], TInt, [], exp) in
  ( fun_def,
    {
      default_attrs with
      cfDest = Return;
      ty = full_fun_to_tabs fun_def;
      effs = [];
    } )

(* Mark children's cf_dest according to the current cf_dest *)
let mark_cf_dest _ ((exp, attrs) as rexp : expr) : expr =
  let mark cf_dest (exp, attrs) = (exp, { attrs with cfDest = cf_dest }) in
  match exp with
  | Var _ | Int _ | Bool _ | Unit -> rexp
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
  | Resume (e, r) -> (Resume (mark attrs.cfDest e, r), attrs)
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
      | Resume (e, r) ->
          Resume
            ( transform_exp_rec state e,
              if r = None then None
              else Some (transform_exp_rec state (Option.get r)) )
      | Seq (e1, e2) ->
          Seq (transform_exp_rec state e1, transform_exp_rec state e2)
      | (Int _ | Bool _ | Unit | Var _) as e -> e),
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
      is_in_general_handler = false;
    }
  in
  exp
  |> transform_exp init_state
       [
         mark_handlers;
         mark_handlerKind;
         transform_general_handler;
         mark_resumer;
         mark_var_depth;
         transform_tail_resumptive_handler;
         mark_cf_dest;
         mark_recursive_call;
         mark_topLevelFunctionName;
         mark_builtin_call;
         expand_hvar_and_funarg;
         add_jb_arg_for_handler;
         add_env_arg_for_fun;
         mark_freeVars;
       ]
       [ update_static_link; update_is_in_general_handler ]
  |> transform_exp init_state [ mark_optimized_sjlj ]
       [ update_curr_func_is_tail_recursive ]
