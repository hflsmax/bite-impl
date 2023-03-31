open Syntax
open Pass_state
open Pass_util
open Util

let mark_handlers state ((exp, attrs) : expr) =
  match exp with
  | Handle (x, fname, (c, cattrs), exp_handle) ->
      ( Handle (x, fname, (c, { cattrs with isHandler = true }), exp_handle),
        attrs )
  | _ -> (exp, attrs)

let mark_resumer state ((exp, attrs) : expr) =
  match exp with
  | Resume (e, _) ->
      if state.is_in_general_handler then
        (Resume (e, Some (Var "r", default_attrs)), attrs)
      else (Resume (e, None), attrs)
  | _ -> (exp, attrs)

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

let mark_handlerKind state ((exp, attrs) : expr) =
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

let get_var_depth (x : name) (slink : string list list) : int =
  let rec get_var_depth' (x : name) (slink : string list list) (depth : int) :
      int =
    match slink with
    | [] -> error "Variable \"%s\" not found in static link@." x
    | locals :: slink' ->
        if List.mem x locals then depth else get_var_depth' x slink' (depth + 1)
  in
  get_var_depth' x slink 0

let mark_var_depth state ((exp, attrs) : expr) =
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

let mark_optimized_sjlj state ((exp, attrs) : expr) =
  match exp with
  | Handle (x, fname, exp_catch, exp_handle) ->
      if state.curr_func_is_tail_recursive then
        (exp, { attrs with isOptimizedSjlj = true })
      else (exp, attrs)
  | _ -> (exp, attrs)

let mark_recursive_call state ((exp, attrs) : expr) : expr =
  match exp with
  | FullApply ((Var x, _), _, _, _) ->
      if x = state.curr_func_name then
        (exp, { attrs with isRecursiveCall = true })
      else (exp, attrs)
  | _ -> (exp, attrs)

let mark_topLevelFunctionName state ((exp, attrs) : expr) : expr =
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
