open Syntax
open Sexplib.Std
open Pass_util

type pass_state = {
  curr_func_name : string;
  curr_func_is_tail_recursive : bool;
  value_store : (string * string) list;
  static_link : string list list;
  eff_defs : f_ENV;
  is_in_general_handler : bool;
}
[@@deriving sexp]

let update_is_in_general_handler state ((exp, attrs) : expr) =
  match exp with
  | FullFun (x, es1, hs, tm_args, ty, es2, exp_body) ->
      if
        attrs.handlerKind = Some Multishot
        || attrs.handlerKind = Some SingleShot
      then { state with is_in_general_handler = true }
      else state
  | _ -> state

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

let update_curr_func_is_tail_recursive state ((exp, attrs) : expr) =
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
