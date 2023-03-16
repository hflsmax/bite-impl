
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

let analyze_handler_kind ((f', _, _, _) as f : R.expr) : handler_kind =
   match f' with
   | FullFun (_, _, _, _, _, _, body) ->
      begin
      match body with
      | Resume _, _, _, _ -> TailResumptive
      | _ -> Abortive
      end
   | _ -> Zoo.error "Handler is not a full function: %t@." (Print.rexpr f)

(* 
   1. record variable depth
   2. record type of fnames
   3. add handler kind to handler 
*)
let enrich_type (eff_defs : f_ENV) (exp : R.expr) : R.expr =
  let rec enrich_type' ((exp, ty, effs, attrs): R.expr) (slink : static_link) : R.expr =
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
    | Handler (k, f) -> (Handler (analyze_handler_kind f, enrich_type' f slink))
    | FullApply (exp, es, hs, exps) ->
       (FullApply (enrich_type' exp slink, es, hs, List.map (fun exp_iter -> enrich_type' exp_iter slink) exps))
    | Raise (h, es, hs, exps) ->
       (Raise (h, es, hs, List.map (fun exp_iter -> enrich_type' exp_iter slink) exps))
    | Resume e -> (Resume (enrich_type' e slink))
    | Seq (e1, e2) ->
       (Seq (enrich_type' e1 slink, enrich_type' e2 slink))
    | Int _  | Bool _  | Abort as e -> e
   end, ty, effs, attrs
  in
    enrich_type' exp (gather_locals exp :: [])

(* If tail-resumptive, remove resume expression such that it's treated like a function
   If abortive, remove resume and insert abort at the end *)
let transform_handler (exp : R.expr') : R.expr' =
   match exp with
   | Handler (k, (f, _, _, _)) ->
      let[@warning "-partial-match"] FullFun (x, es1, hs, tm_args, ty, es2, (exp_body, exp_body_ty, exp_body_es, exp_body_attrs)) = f in
      begin
      match k with
      | TailResumptive ->
         let[@warning "-partial-match"] Resume exp_body' = exp_body in
         FullFun (x, es1, hs, tm_args, ty, es2, exp_body')
      | Abortive ->
         FullFun (x, es1, hs, tm_args, ty, es2, (R.Seq ((exp_body, exp_body_ty, exp_body_es, exp_body_attrs), (Abort, TInt, [], default_attrs)), exp_body_ty, exp_body_es, exp_body_attrs))
      | Other -> 
         Zoo.error "Other handler kind not supported"
      end
   | _ -> exp

let mark_tail_call fun_name ((exp, ty, effs, attrs) : R.expr) : R.expr =
   match exp with
   | FullApply ((Var (_, x), _, _, _), _, _, _) ->
      if x = fun_name then
         (exp, ty, effs, {attrs with isTailCall = true})
      else
         (exp, ty, effs, attrs)
   | _ -> (exp, ty, effs, attrs)

(* Bottom-up AST walker *)
let transform_exp (exp : R.expr) : R.expr =
 let curr_func_name = ref "" in
  let rec transform_exp' ((exp, ty, effs, attrs) as rexp: R.expr) : R.expr =
  (* Add pre-metadata-updater here. *)
  begin
  match exp with
  | FullFun (x, _, _, _, _, _, _) -> curr_func_name := x
  | _ -> ()
  end;
  (* Add pre-transformer here. This corresponds to a top-dowm transformation *)
  let (exp_pre, ty_pre, effs_pre, attrs_pre) as rexp_pre = 
    (mark_tail_call !curr_func_name) rexp 
  in
      let exp_walked = 
         match exp_pre with
         | Times (e1, e2) -> (Times (transform_exp' e1, transform_exp' e2))
         | Plus (e1, e2) -> (Plus (transform_exp' e1, transform_exp' e2))
         | Minus (e1, e2) -> (Minus (transform_exp' e1, transform_exp' e2))
         | Equal (e1, e2) -> (Equal (transform_exp' e1, transform_exp' e2))
         | Less (e1, e2) -> (Less (transform_exp' e1, transform_exp' e2))
         | Assign (e1, e2) -> (Assign (transform_exp' e1, transform_exp' e2))
         | Deref e -> (Deref (transform_exp' e))
         | If (e1, e2, e3) ->
            (If (transform_exp' e1, transform_exp' e2, transform_exp' e3))
         | Let (x, ty, e1, e2) ->
            (Let (x, ty, transform_exp' e1, transform_exp' e2))
         | Decl (x, ty, e1, e2) ->
            (Decl (x, ty, transform_exp' e1, transform_exp' e2))
         | Handle (x, h, exp_catch, exp_handle) ->
            (Handle (x, h, transform_exp' exp_catch, transform_exp' exp_handle))
         | FullFun (x, es1, hs, tm_args, ty, es2, exp_body) ->
            (FullFun (x, es1, hs, tm_args, ty, es2, transform_exp' exp_body))
         | Handler (k, f) -> (Handler (k, transform_exp' f))
         | FullApply (exp, es, hs, exps) ->
            (FullApply (transform_exp' exp, es, hs, List.map (fun exp_iter -> transform_exp' exp_iter) exps))
         | Raise (h, es, hs, exps) ->
            (Raise (h, es, hs, List.map (fun exp_iter -> transform_exp' exp_iter) exps))
         | Resume e -> (Resume (transform_exp' e))
         | Seq (e1, e2) ->
            (Seq (transform_exp' e1, transform_exp' e2))
         | Int _  | Bool _ | Var _ | Abort as e -> e
      in
      (* Add post-transformer here. This corresponds to a bottom-up transformation *)
      transform_handler exp_walked, ty_pre, effs_pre, attrs_pre
  in
  transform_exp' exp