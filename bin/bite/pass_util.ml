open Syntax

[@@@ocaml.warning "-unused-open"]

open Util

let gather_exp include_all_lexical_scope predicate exp =
  let rec gather_exp' (exp, attrs) =
    (if predicate (exp, attrs) then [ exp ] else [])
    @
    match exp with
    | AOP (_, e1, e2) | BOP (_, e1, e2) -> gather_exp' e1 @ gather_exp' e2
    | UOP (_, e) -> gather_exp' e
    | Assign (e1, e2) -> gather_exp' e1 @ gather_exp' e2
    | Deref e -> gather_exp' e
    | If (e1, e2, e3) -> gather_exp' e1 @ gather_exp' e2 @ gather_exp' e3
    | Let (x, _, e1, e2) -> gather_exp' e1 @ gather_exp' e2
    | Decl (x, _, e1, e2) -> gather_exp' e1 @ gather_exp' e2
    | Handle (x, h, exp_catch, exp_handle) ->
        gather_exp' exp_catch @ gather_exp' exp_handle
    | FullFun (x, es1, hs, tm_args, ty, es2, exp_body) ->
        if include_all_lexical_scope then gather_exp' exp_body else []
    | FullApply (exp, es, hs, exps) ->
        gather_exp' exp @ List.concat (List.map gather_exp' exps)
    | Raise (h, es, hs, exps) -> List.concat (List.map gather_exp' exps)
    | Resume (e, r) -> gather_exp' e
    | Seq (e1, e2) -> gather_exp' e1 @ gather_exp' e2
    | Int _ | Bool _ | Aux _ | Unit | Var _ -> []
  in
  gather_exp' exp

(* Gather all free variables in an expression. It's computed by finding all used variables that are not bound *)
(* if rich hvars are not populated, the type of hvar will be TUnit *)
let rec gather_free_vars ?(exclude_names = []) ((exp, attrs) : expr) : locals =
  let exclude name locals =
    List.filter (fun (name', _) -> name <> name') locals
  in
  let exclude_all names locals =
    List.filter (fun (name', _) -> not (List.mem name' names)) locals
  in
  match exp with
  | AOP (_, e1, e2) | BOP (_, e1, e2) ->
      gather_free_vars e1 @ gather_free_vars e2
  | UOP (_, e) -> gather_free_vars e
  | Assign (x, e) ->
      let[@warning "-partial-match"] Var name, attrs = x in
      (name, attrs.ty) :: gather_free_vars e
  | If (e1, e2, e3) ->
      gather_free_vars e1 @ gather_free_vars e2 @ gather_free_vars e3
  | Let (x, _, e1, e2) -> gather_free_vars e1 @ exclude x (gather_free_vars e2)
  | Decl (x, _, e1, e2) -> gather_free_vars e1 @ exclude x (gather_free_vars e2)
  | Handle (x, fname, catch_exp, handle_exp) ->
      gather_free_vars catch_exp @ exclude x (gather_free_vars handle_exp)
  | FullFun (x, _, hparams, tparams, _, _, body) ->
      let hparams' = List.map fst hparams in
      let tparams' = List.map fst tparams in
      exclude_all ((x :: hparams') @ tparams') (gather_free_vars body)
  | FullApply (lhs, _, hvars, targs) ->
      gather_free_vars lhs
      @ List.fold_left
          (fun acc exp_iter -> acc @ gather_free_vars exp_iter)
          [] targs
  | Raise (name, _, hargs, targs) ->
      List.fold_left
        (fun acc exp_iter -> acc @ gather_free_vars exp_iter)
        [] targs
  | Resume (e, _) -> gather_free_vars e
  | Seq (e1, e2) -> gather_free_vars e1 @ gather_free_vars e2
  | Deref x -> gather_free_vars x
  | Var x ->
      if
        attrs.isBuiltin || List.mem x exclude_names
        || Option.get attrs.varDepth = -1
      then []
      else [ (x, attrs.ty) ]
  | Int _ | Bool _ | Unit | Aux _ -> [] |> List.sort_uniq compare

let get_all_func_names exp =
  gather_exp true
    (fun (exp, _) -> match exp with FullFun _ -> true | _ -> false)
    exp
  |> List.map
       (fun [@warning "-partial-match"] (FullFun (name, _, _, _, _, _, _)) ->
         name)
