
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