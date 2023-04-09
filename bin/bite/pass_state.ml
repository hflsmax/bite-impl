open Syntax
open Pass_util

[@@@ocaml.warning "-unused-open"]

open Util

type pass_state = {
  func_names : string list;
  curr_func_names : string list;
  curr_func_is_tail_recursive : bool;
  value_store : (string * string) list;
  static_link : (string * bool) list list; (* name, isTop *)
  eff_defs : f_ENV;
  is_in_general_handler : bool;
}
[@@deriving yojson_of]

let update_is_in_general_handler state ((exp, attrs) : expr) =
  match exp with
  | FullFun (x, es1, hs, tm_args, ty, es2, exp_body) ->
      if
        attrs.handlerKind = Some MultiShot
        || attrs.handlerKind = Some SingleShot
      then { state with is_in_general_handler = true }
      else state
  | _ -> state

let update_static_link state ((exp, attrs) : expr) =
  match exp with
  | FullFun (fun_name, es1, hs, tm_args, ty, es2, exp_body) ->
      let locals = List.map fst tm_args @ List.map fst hs in
      {
        state with
        static_link =
          ((fun_name, true) :: List.map (fun x -> (x, false)) locals)
          :: state.static_link;
      }
  | Let (x, isTop, e1, e2) ->
      {
        state with
        static_link =
          ((x, isTop) :: List.hd state.static_link) :: List.tl state.static_link;
      }
  | Decl (x, isTop, e1, e2) ->
      {
        state with
        static_link =
          ((x, isTop) :: List.hd state.static_link) :: List.tl state.static_link;
      }
  | Handle (x, fname, exp_catch, exp_handle) ->
      {
        state with
        static_link =
          ((x, false) :: List.hd state.static_link) :: List.tl state.static_link;
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
                   x = x' && attrs.cfDest <> Return
               | _ -> false)
             exp_body)
        != 0
      then { state with curr_func_is_tail_recursive = false }
      else { state with curr_func_is_tail_recursive = true }
  | _ -> state
