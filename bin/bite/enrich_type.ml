
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

let rec record_depth (exp : R.expr) : R.expr =
  let rec record_depth' ((exp, ty, effs): R.expr) (slink : static_link) : R.expr =
   begin
    match exp with
    | Var (_, x) -> (Var ((get_var_depth x slink), x))
    | Times (e1, e2) -> (Times (record_depth' e1 slink, record_depth' e2 slink))
    | Plus (e1, e2) -> (Plus (record_depth' e1 slink, record_depth' e2 slink))
    | Minus (e1, e2) -> (Minus (record_depth' e1 slink, record_depth' e2 slink))
    | Equal (e1, e2) -> (Equal (record_depth' e1 slink, record_depth' e2 slink))
    | Less (e1, e2) -> (Less (record_depth' e1 slink, record_depth' e2 slink))
    | Assign (e1, e2) -> (Assign (record_depth' e1 slink, record_depth' e2 slink))
    | Deref e -> (Deref (record_depth' e slink))
    | If (e1, e2, e3) ->
       (If (record_depth' e1 slink, record_depth' e2 slink, record_depth' e3 slink))
    | Let (x, ty, e1, e2) ->
       (Let (x, ty, record_depth' e1 slink, record_depth' e2 slink))
    | Decl (x, ty, e1, e2) ->
       (Decl (x, ty, record_depth' e1 slink, record_depth' e2 slink))
    | Handle (x, fname, exp_catch, exp_handle) ->
       (Handle (x, fname, record_depth' exp_catch slink, record_depth' exp_handle slink))
    | FullFun (x, es1, hs, tm_args, ty, es2, exp_body) ->
      let hd_args = List.map (fun (name, _, ty) -> (name, ty)) hs in
        let locals = (x, full_fun_to_tabs exp) :: tm_args @ hd_args @ gather_locals exp_body in
         (FullFun (x, es1, hs, tm_args, ty, es2, record_depth' exp_body (locals :: slink)))
    | Handler (k, f) -> (Handler (k, record_depth' f slink))
    | FullApply (exp, es, hs, exps) ->
       (FullApply (record_depth' exp slink, es, hs, List.map (fun exp_iter -> record_depth' exp_iter slink) exps))
    | Raise (h, es, hs, exps) ->
       (Raise (h, es, hs, List.map (fun exp_iter -> record_depth' exp_iter slink) exps))
    | Resume e -> (Resume (record_depth' e slink))
    | Seq (e1, e2) ->
       (Seq (record_depth' e1 slink, record_depth' e2 slink))
    | Int _  | Bool _  as e -> e
   end, ty, effs
  in
    record_depth' exp (gather_locals exp :: [])

let rec record_fname_ty (eff_defs : f_ENV) ((exp, ty, eff) : expr) : expr =
   begin
   match exp with
   | Var _  | Int _  | Bool _  as e -> e
   | Times (e1, e2) -> (Times (record_fname_ty eff_defs e1, record_fname_ty eff_defs e2))
   | Plus (e1, e2) -> (Plus (record_fname_ty eff_defs e1, record_fname_ty eff_defs e2))
   | Minus (e1, e2) -> (Minus (record_fname_ty eff_defs e1, record_fname_ty eff_defs e2))
   | Equal (e1, e2) -> (Equal (record_fname_ty eff_defs e1, record_fname_ty eff_defs e2))
   | Less (e1, e2) -> (Less (record_fname_ty eff_defs e1, record_fname_ty eff_defs e2))
   | Assign (e1, e2) -> (Assign (record_fname_ty eff_defs e1, record_fname_ty eff_defs e2))
   | Deref e -> (Deref (record_fname_ty eff_defs e))
   | If (e1, e2, e3) ->
      (If (record_fname_ty eff_defs e1, record_fname_ty eff_defs e2, record_fname_ty eff_defs e3))
   | Let (x, ty, e1, e2) ->
      (Let (x, ty, record_fname_ty eff_defs e1, record_fname_ty eff_defs e2))
   | Decl (x, ty, e1, e2) ->
      (Decl (x, ty, record_fname_ty eff_defs e1, record_fname_ty eff_defs e2))
   | Handle (x, (fname, _), exp_catch, exp_handle) ->
      (Handle (x, (fname, List.assoc fname eff_defs), record_fname_ty eff_defs exp_catch, record_fname_ty eff_defs exp_handle))
   | FullFun (x, es1, hs, tm_args, ty, es2, exp) ->
      let hs' = List.map (fun (x, fname, _) -> (x, fname, List.assoc fname eff_defs)) hs in
      FullFun (x, es1, hs', tm_args, ty, es2, record_fname_ty eff_defs exp)
   | Handler (k, e) -> Handler (k, record_fname_ty eff_defs e)
   | FullApply (exp, es, hs, exps) ->
      (FullApply (record_fname_ty eff_defs exp, es, hs, List.map (fun exp_iter -> record_fname_ty eff_defs exp_iter) exps))
   | Raise (h, es, hs, exps) ->
      (Raise (h, es, hs, List.map (fun exp_iter -> record_fname_ty eff_defs exp_iter) exps))
   | Resume e -> (Resume (record_fname_ty eff_defs e))
   | Seq (e1, e2) ->
      (Seq (record_fname_ty eff_defs e1, record_fname_ty eff_defs e2))
   end, ty, eff