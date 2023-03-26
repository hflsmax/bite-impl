open Syntax.R

let gather_exp predicate exp =
  let rec gather_exp' (exp, _, _, _) =
    (if predicate exp then [ exp ] else [])
    @
    match exp with
    | Times (e1, e2) -> gather_exp' e1 @ gather_exp' e2
    | Plus (e1, e2) -> gather_exp' e1 @ gather_exp' e2
    | Minus (e1, e2) -> gather_exp' e1 @ gather_exp' e2
    | Equal (e1, e2) -> gather_exp' e1 @ gather_exp' e2
    | Less (e1, e2) -> gather_exp' e1 @ gather_exp' e2
    | Assign (e1, e2) -> gather_exp' e1 @ gather_exp' e2
    | Deref e -> gather_exp' e
    | If (e1, e2, e3) -> gather_exp' e1 @ gather_exp' e2 @ gather_exp' e3
    | Let (x, ty, e1, e2) -> gather_exp' e1 @ gather_exp' e2
    | Decl (x, ty, e1, e2) -> gather_exp' e1 @ gather_exp' e2
    | Handle (x, h, exp_catch, exp_handle) ->
        gather_exp' exp_catch @ gather_exp' exp_handle
    | FullFun (kind, x, es1, hs, tm_args, ty, es2, exp_body) -> []
    | FullApply (exp, es, hs, exps) ->
        gather_exp' exp @ List.concat (List.map gather_exp' exps)
    | Raise (h, es, hs, exps) -> List.concat (List.map gather_exp' exps)
    | Resume e -> gather_exp' e
    | Seq (e1, e2) -> gather_exp' e1 @ gather_exp' e2
    | Int _ | Bool _ | Var _ -> []
  in
  gather_exp' exp
