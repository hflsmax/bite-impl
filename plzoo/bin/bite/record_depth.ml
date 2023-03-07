
open Syntax

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

let rec record_depth (eff_defs : f_ENV) (exp : expr) : expr =
  let rec record_depth' (eff_defs : f_ENV) ({Zoo.data=exp; loc} : expr) (slink : static_link) : expr =
    Zoo.locate ~loc @@
    match exp with
    | Var (_, x) -> (Var ((get_var_depth ~loc x slink), x))
    | Times (e1, e2) -> (Times (record_depth' eff_defs e1 slink, record_depth' eff_defs e2 slink))
    | Plus (e1, e2) -> (Plus (record_depth' eff_defs e1 slink, record_depth' eff_defs e2 slink))
    | Minus (e1, e2) -> (Minus (record_depth' eff_defs e1 slink, record_depth' eff_defs e2 slink))
    | Equal (e1, e2) -> (Equal (record_depth' eff_defs e1 slink, record_depth' eff_defs e2 slink))
    | Less (e1, e2) -> (Less (record_depth' eff_defs e1 slink, record_depth' eff_defs e2 slink))
    | Assign (e1, e2) -> (Assign (record_depth' eff_defs e1 slink, record_depth' eff_defs e2 slink))
    | Deref e -> (Deref (record_depth' eff_defs e slink))
    | If (e1, e2, e3) ->
       (If (record_depth' eff_defs e1 slink, record_depth' eff_defs e2 slink, record_depth' eff_defs e3 slink))
    | Let (x, ty, e1, e2) ->
       (Let (x, ty, record_depth' eff_defs e1 slink, record_depth' eff_defs e2 slink))
    | Decl (x, ty, e1, e2) ->
       (Decl (x, ty, record_depth' eff_defs e1 slink, record_depth' eff_defs e2 slink))
    | Handle (x, fname, exp_catch, exp_handle) ->
       (Handle (x, fname, record_depth' eff_defs exp_catch slink, record_depth' eff_defs exp_handle slink))
    | FullFun (x, es1, hs, tm_args, ty, es2, exp) ->
      let ty_args = List.map snd tm_args in
        let locals = (x, TAbs (es1, hs, ty_args, ty, es2)) :: tm_args @ gather_locals eff_defs exp in
         (FullFun (x, es1, hs, tm_args, ty, es2, record_depth' eff_defs exp (locals :: slink)))
    | FullApply (exp, es, hs, exps) ->
       (FullApply (record_depth' eff_defs exp slink, es, hs, List.map (fun exp_iter -> record_depth' eff_defs exp_iter slink) exps))
    | Raise (h, es, hs, exps) ->
       (Raise (h, es, hs, List.map (fun exp_iter -> record_depth' eff_defs exp_iter slink) exps))
    | Seq (e1, e2) ->
       (Seq (record_depth' eff_defs e1 slink, record_depth' eff_defs e2 slink))
    | Int _  | Bool _  as e -> e
  in
    record_depth' eff_defs exp (gather_locals eff_defs exp :: [])