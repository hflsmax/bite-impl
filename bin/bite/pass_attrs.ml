open Syntax
open Pass_state
open Pass_util
open Common

[@@@ocaml.warning "-unused-open"]

open Util

let mark_freeVars state ((exp, attrs) : expr) =
  match exp with
  | FullFun (x, _, _, _, _, _, body) ->
      ( exp,
        {
          attrs with
          freeVars = gather_free_vars (exp, attrs);
          freeVarsOfBody = gather_free_vars body;
        } )
  | _ -> (exp, attrs)

let mark_handlers state ((exp, attrs) : expr) =
  match exp with
  | Handle (x, fname, (c, cattrs), exp_handle) ->
      ( Handle (x, fname, (c, { cattrs with isHandler = true }), exp_handle),
        attrs )
  | _ -> (exp, attrs)

(*
    TailResumptive: the whole body is a Resume expression
    Abortive: the handler doesn't have continuation as an argument
    SingleShot: the handler takes continuation as the last argument *)
let analyze_handlerKind (f : expr') : handlerKind =
  match f with
  | FullFun (fun_name, _, _, term_args, _, _, body) ->
      let has_resume =
        List.length
          (gather_exp true
             (fun exp -> match exp with Resume _, _ -> true | _ -> false)
             body)
        > 0
      in
      let wrapped_by_resume =
        match body with Resume _, _ -> true | _ -> false
      in
      let cont_as_last_arg =
        List.length term_args > 0
        && match snd (get_last term_args) with TCont _ -> true | _ -> false
      in
      if has_resume && wrapped_by_resume && not cont_as_last_arg then
        TailResumptive
      else if (not has_resume) && not cont_as_last_arg then Abortive
      else if cont_as_last_arg then SingleShot
      else error "Fail to recognize handler kind: %s@." fun_name
  | _ -> error "Handler is not a full function: %t@." (Print.expr' f)

let mark_handlerKind state ((exp, attrs) : expr) =
  match exp with
  | Handle (x, fname, (f, fattrs), exp_handle) ->
      let analyzed_kind = analyze_handlerKind f in
      let declared_kind = (Option.get attrs.bindHvar).kind in
      if analyzed_kind = declared_kind then
        ( Handle
            ( x,
              fname,
              (f, { fattrs with handlerKind = Some analyzed_kind }),
              exp_handle ),
          { attrs with handlerKind = Some analyzed_kind } )
      else
        error
          "declared handlerKind does not match implementation. Expected %t, \
           got %t@."
          (Print.handlerKind declared_kind)
          (Print.handlerKind analyzed_kind)
  | _ -> (exp, attrs)

let mark_var_depth state ((exp, attrs) : expr) =
  let rec get_var_depth_rec (x : name) (slink : (string * bool) list list)
      (depth : int) : int option =
    match slink with
    | [] ->
        if List.mem x state.func_names then (
          print_string ".is func_name.";
          Some (-1))
        else None
    | locals :: slink' -> (
        match List.assoc_opt x locals with
        | Some isTop ->
            if isTop then (
              print_string ".is top.";
              Some (-1))
            else Some depth
        | None -> get_var_depth_rec x slink' (depth + 1))
  in
  match exp with
  | Var x -> (
      Util.print_info "@.@.Marking var depth for %s@.slink:%t@." x
        (Print.static_link state.static_link);
      if attrs.isBuiltin then (exp, attrs)
      else
        match get_var_depth_rec x state.static_link 0 with
        | Some depth ->
            Util.print_info "Marking var depth for %s as %d@." x depth;
            (exp, { attrs with varDepth = Some depth })
        | None ->
            error "Variable %s not found in static link %t@." x
              (Print.static_link state.static_link)
              ~loc:attrs.loc)
  (* | Raise (x, _, hvars, _) ->
         let hvarArgs' =
           List.map
             (fun hvar ->
               Util.print_info "Marking var depth for %s@." hvar.name;
               let depth = get_var_depth hvar.name state.static_link in
               { hvar with depth })
             (Option.get attrs.hvarArgs)
         in
         Util.print_info "Marking var depth for %s@." x;
         let depth = get_var_depth x state.static_link in
         ( exp,
           {
             attrs with
             lhsHvar = Some { (Option.get attrs.lhsHvar) with depth };
             hvarArgs = Some hvarArgs';
           } )
     | FullApply (_, _, hvars, _) ->
         let hvarArgs' =
           List.map
             (fun hvar ->
               Util.print_info "Marking var depth for %s@. in%t@." hvar.name (Print.expr exp);
               let depth = get_var_depth hvar.name state.static_link in
               { hvar with depth })
             (Option.get attrs.hvarArgs)
         in
         (exp, { attrs with hvarArgs = Some hvarArgs' }) *)
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
      if List.mem x state.curr_func_names then
        (exp, { attrs with recursiveCallFunName = Some x })
      else (exp, attrs)
  | _ -> (exp, attrs)

let mark_builtin_call _ ((exp, attrs) : expr) : expr =
  match exp with
  | FullApply ((_, x_attrs), _, _, _) ->
      if x_attrs.isBuiltin then (exp, { attrs with isBuiltin = true })
      else (exp, attrs)
  | _ -> (exp, attrs)

let mark_unnecessary_reify_env state ((exp, attrs) : expr) : expr =
  match exp with
  | Let (x, isTop, ((FullFun _, fattrs) as fexpr), e2) ->
      let mark_rec (exp, attrs) =
        ( exp,
          match exp with
          | Let (x, isTop, ((Aux ReifyEnvironment, _) as rexpr), e2) ->
              { attrs with skipDef = fattrs.freeVars = [] }
          | Let (x, isTop, ((Aux _, _) as rexpr), e2) -> attrs
          | _ -> attrs )
      in
      (Let (x, isTop, fexpr, mark_rec e2), attrs)
  | _ -> (exp, attrs)
