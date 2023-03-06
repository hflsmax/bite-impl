module SR = Syntax
open Syntax_hoist_locals

type locals = (name * ty) list
type static_link = locals list

let rec hoist_locals ({Zoo.data=exp; _} : SR.expr) : expr =
  let rec gather_locals (exp : SR.expr') : locals =
    match exp with
    | Times ({Zoo.data=e1; _}, {Zoo.data=e2; _}) | Plus ({Zoo.data=e1; _}, {Zoo.data=e2; _}) | Minus ({Zoo.data=e1; _}, {Zoo.data=e2; _})
    | Equal ({Zoo.data=e1; _}, {Zoo.data=e2; _}) | Less ({Zoo.data=e1; _}, {Zoo.data=e2; _}) ->
        gather_locals e1 @ gather_locals e2
    (* | Assign (x, {Zoo.data=e; _}) -> (x, type_of e) :: gather_locals e *)
    | _ -> []
  in
  let rec hoist_locals' (ex : SR.expr') (env : static_link) : expr =
    match exp with
    | SR.Var x -> Var x
    | _ -> Var "TODO"
  in
    hoist_locals' exp []