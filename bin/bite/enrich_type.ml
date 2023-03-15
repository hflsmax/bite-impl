
open Syntax
open Syntax.R
open Common
let get_var_depth (x : name) (slink : static_link) : int =
  let rec get_var_depth' (x : name) (slink : static_link) (depth : int) : int =
    match slink with
    | [] -> Zoo.error "Variable \"%s\" not found in static link: %t@." x (Print.static_link slink)
    | locals :: slink' ->
        if List.mem_assoc x locals then depth
        else get_var_depth' x slink' (depth + 1)
  in
    get_var_depth' x slink 0

(* record variable depth and record type of fnames *)
let rec enrich_type (eff_defs : f_ENV) (exp : R.expr) : R.expr =
  let rec enrich_type' ((exp, ty, effs): R.expr) (slink : static_link) : R.expr =
   begin
    match exp with
    | Var (_, x) -> (Var ((get_var_depth x slink), x))
    | Times (e1, e2) -> (Times (enrich_type' e1 slink, enrich_type' e2 slink))
    | Plus (e1, e2) -> (Plus (enrich_type' e1 slink, enrich_type' e2 slink))
    | Minus (e1, e2) -> (Minus (enrich_type' e1 slink, enrich_type' e2 slink))
    | Equal (e1, e2) -> (Equal (enrich_type' e1 slink, enrich_type' e2 slink))
    | Less (e1, e2) -> (Less (enrich_type' e1 slink, enrich_type' e2 slink))
    | Assign (e1, e2) -> (Assign (enrich_type' e1 slink, enrich_type' e2 slink))
    | Deref e -> (Deref (enrich_type' e slink))
    | If (e1, e2, e3) ->
       (If (enrich_type' e1 slink, enrich_type' e2 slink, enrich_type' e3 slink))
    | Let (x, ty, e1, e2) ->
       (Let (x, ty, enrich_type' e1 slink, enrich_type' e2 slink))
    | Decl (x, ty, e1, e2) ->
       (Decl (x, ty, enrich_type' e1 slink, enrich_type' e2 slink))
    | Handle (x, (fname, _), exp_catch, exp_handle) ->
       (Handle (x, (fname, List.assoc fname eff_defs), enrich_type' exp_catch slink, enrich_type' exp_handle slink))
    | FullFun (x, es1, hs, tm_args, ty, es2, exp_body) ->
      let hs' = List.map (fun (x, fname, _) -> (x, fname, List.assoc fname eff_defs)) hs in
      let hd_args = List.map (fun (name, _, ty) -> (name, ty)) hs in
        let locals = (x, full_fun_to_tabs exp) :: tm_args @ hd_args @ gather_locals exp_body in
         (FullFun (x, es1, hs', tm_args, ty, es2, enrich_type' exp_body (locals :: slink)))
    | Handler (k, f) -> (Handler (k, enrich_type' f slink))
    | FullApply (exp, es, hs, exps) ->
       (FullApply (enrich_type' exp slink, es, hs, List.map (fun exp_iter -> enrich_type' exp_iter slink) exps))
    | Raise (h, es, hs, exps) ->
       (Raise (h, es, hs, List.map (fun exp_iter -> enrich_type' exp_iter slink) exps))
    | Resume e -> (Resume (enrich_type' e slink))
    | Seq (e1, e2) ->
       (Seq (enrich_type' e1 slink, enrich_type' e2 slink))
    | Int _  | Bool _  as e -> e
   end, ty, effs
  in
    enrich_type' exp (gather_locals exp :: [])
