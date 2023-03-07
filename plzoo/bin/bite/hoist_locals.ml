
open Syntax

(* let rec trans_ty (ty : SR.ty) : ty =
  match ty with
  | SR.TInt -> TInt
  | SR.TBool -> TBool
  (* TODO *)
  | SR.TAbs (es1, hs, ts, ty, es2) -> TBool
  | SR.TMut ty -> TMut (trans_ty ty) *)

let rec gather_locals (eff_defs : f_ENV) ({Zoo.data=exp; loc} : expr) : locals =
  match exp with
  | Times (e1, e2) | Plus (e1, e2) | Minus (e1, e2)
  | Equal (e1, e2) | Less (e1, e2) ->
      gather_locals eff_defs e1 @ gather_locals eff_defs e2
  | Assign (x, e) -> gather_locals eff_defs e
  | If (e1, e2, e3) ->
      gather_locals eff_defs e1 @ gather_locals eff_defs e2 @ gather_locals eff_defs e3
  | Let (x, ty, e1, e2) ->
      (x, ty) :: gather_locals eff_defs e1 @ gather_locals eff_defs e2
  | Decl (x, ty, e1, e2) ->
      (x, TMut ty) :: gather_locals eff_defs e1 @ gather_locals eff_defs e2
  | Handle (x, fname, _, e2) ->
      let ty = List.assoc fname eff_defs in
        (x, ty) :: gather_locals eff_defs e2
  | FullApply (e1, _, _, e2) -> 
      gather_locals eff_defs e1 @ List.fold_left (fun acc exp_iter -> acc @ (gather_locals eff_defs exp_iter)) [] e2
  | Raise (_, _, _, e) ->
      List.fold_left (fun acc exp_iter -> acc @ (gather_locals eff_defs exp_iter)) [] e
  | Seq (e1, e2) -> gather_locals eff_defs e1 @ gather_locals eff_defs e2
  | Var _ | Int _ | Bool _ | Deref _ | FullFun _ -> []

let get_var_depth ~loc (x : name) (slink : static_link) : int =
  let rec get_var_depth' (x : name) (slink : static_link) (depth : int) : int =
    match slink with
    | [] -> Zoo.error ~loc "Variable \"%s\" not found in static link: %t@." x (Print.static_link slink)
    | locals :: slink' ->
        if List.mem_assoc x locals then depth
        else get_var_depth' x slink' (depth + 1)
  in
    get_var_depth' x slink 0

let rec hoist_locals (eff_defs : f_ENV) (exp : expr) : expr =
  let rec hoist_locals' (eff_defs : f_ENV) ({Zoo.data=exp; loc} : expr) (slink : static_link) : expr =
    match exp with
    | Var (_, x) -> Zoo.locate ~loc (Var ((get_var_depth ~loc x slink), x))
    | Times (e1, e2) -> Zoo.locate ~loc (Times (hoist_locals' eff_defs e1 slink, hoist_locals' eff_defs e2 slink))
    | Plus (e1, e2) -> Zoo.locate ~loc (Plus (hoist_locals' eff_defs e1 slink, hoist_locals' eff_defs e2 slink))
    | Minus (e1, e2) -> Zoo.locate ~loc (Minus (hoist_locals' eff_defs e1 slink, hoist_locals' eff_defs e2 slink))
    | Equal (e1, e2) -> Zoo.locate ~loc (Equal (hoist_locals' eff_defs e1 slink, hoist_locals' eff_defs e2 slink))
    | Less (e1, e2) -> Zoo.locate ~loc (Less (hoist_locals' eff_defs e1 slink, hoist_locals' eff_defs e2 slink))
    | Assign (e1, e2) -> Zoo.locate ~loc (Assign (hoist_locals' eff_defs e1 slink, hoist_locals' eff_defs e2 slink))
    | Deref e -> Zoo.locate ~loc (Deref (hoist_locals' eff_defs e slink))
    | If (e1, e2, e3) ->
        Zoo.locate ~loc (If (hoist_locals' eff_defs e1 slink, hoist_locals' eff_defs e2 slink, hoist_locals' eff_defs e3 slink))
    | Let (x, ty, e1, e2) ->
        Zoo.locate ~loc (Let (x, ty, hoist_locals' eff_defs e1 slink, hoist_locals' eff_defs e2 slink))
    | Decl (x, ty, e1, e2) ->
        Zoo.locate ~loc (Decl (x, ty, hoist_locals' eff_defs e1 slink, hoist_locals' eff_defs e2 slink))
    | Handle (x, fname, exp_catch, exp_handle) ->
        Zoo.locate ~loc (Handle (x, fname, hoist_locals' eff_defs exp_catch slink, hoist_locals' eff_defs exp_handle slink))
    | FullFun (x, es1, hs, tm_args, ty, es2, exp) ->
      let ty_args = List.map snd tm_args in
        let locals = (x, TAbs (es1, hs, ty_args, ty, es2)) :: tm_args @ gather_locals eff_defs exp in
          Zoo.locate ~loc (FullFun (x, es1, hs, tm_args, ty, es2, hoist_locals' eff_defs exp (locals :: slink)))
    | FullApply (exp, es, hs, exps) ->
        Zoo.locate ~loc (FullApply (hoist_locals' eff_defs exp slink, es, hs, List.map (fun exp_iter -> hoist_locals' eff_defs exp_iter slink) exps))
    | Raise (h, es, hs, exps) ->
        Zoo.locate ~loc (Raise (h, es, hs, List.map (fun exp_iter -> hoist_locals' eff_defs exp_iter slink) exps))
    | Seq (e1, e2) ->
        Zoo.locate ~loc (Seq (hoist_locals' eff_defs e1 slink, hoist_locals' eff_defs e2 slink))
    | Int _  | Bool _  as e -> Zoo.locate ~loc e
  in
    hoist_locals' eff_defs exp (gather_locals eff_defs exp :: [])