(** Type checking. *)

open Syntax

let typing_error ~loc = Zoo.error ~kind:"Type error" ~loc

let fname_ok (eff_defs : f_ENV) (fname : fname) =
  if not (List.mem_assoc fname eff_defs) then
    Zoo.error ~kind:"Type error" "unknown effect %s" fname

let hd_ok (h_env : h_ENV) (HVar s : hd) =
  if not (List.mem_assoc (HVar s) h_env) then
    Zoo.error ~kind:"Type error" "unknown handler %s" s

let eff_ok (e_env : e_ENV) (h_env : h_ENV) (e : eff) =
    match e with
      EVar s -> if not (List.mem e e_env) then
        Zoo.error ~kind:"Type error" "unknown effect var %s" s
    | Handler (HVar s) -> if not (List.mem_assoc (HVar s) h_env) then
        Zoo.error ~kind:"Type error" "unknown handler var %s" s
    
let rec ty_ok (eff_defs : f_ENV) (e_env : e_ENV) (h_env : h_ENV) ty =
  match ty with
    | TAbs (es1, hs, ts, t, es2) ->
        List.iter (ty_ok eff_defs (es1 @ e_env) (hs @ h_env)) ts ;
        List.iter (fname_ok eff_defs) (snd (List.split hs)) ;
        ty_ok eff_defs (es1 @ e_env) (hs @ h_env) t ;
        List.iter (eff_ok (es1 @ e_env) (hs @ h_env)) es2
    | _ -> ()


let rec check (eff_defs : f_ENV) (e_env : e_ENV) (h_env : h_ENV) (t_env : t_ENV) (ty, es : ty * effs) ({Zoo.loc;_} as e) =
  let ty', es' = type_of eff_defs e_env h_env t_env e in
    if ty' <> ty then
      typing_error ~loc "This expression has type %t but is used as if it has type %t" 
        (Print.ty ty') (Print.ty ty)
    else if not (List.for_all (fun e -> List.mem e es) es') then
      typing_error ~loc "This expression has effects %t but is used as if it has effects %t" 
        (Print.effs es') (Print.effs es)
(* TODO: enforce A-normal form *)
and type_of (eff_defs : f_ENV) (e_env : e_ENV) (h_env : h_ENV) (t_env : t_ENV) {Zoo.data=e; loc} : (ty * effs) =
  match e with
    | Var x ->
      (try (List.assoc x t_env, []) with
	     Not_found -> typing_error ~loc "unknown variable %s" x)
    | Int _ -> (TInt, [])
    | Bool _ -> (TBool, [])
    | Unit -> (TUnit, [])
    | Times (e1, e2) -> check eff_defs e_env h_env t_env (TInt, []) e1 ; check eff_defs e_env h_env t_env (TInt, []) e2 ; (TInt, [])
    | Plus (e1, e2) -> check eff_defs e_env h_env t_env (TInt, []) e1 ; check eff_defs e_env h_env t_env (TInt, []) e2 ; (TInt, [])
    | Minus (e1, e2) -> check eff_defs e_env h_env t_env (TInt, []) e1 ; check eff_defs e_env h_env t_env (TInt, []) e2 ; (TInt, [])
    | Equal (e1, e2) -> check eff_defs e_env h_env t_env (TInt, []) e1 ; check eff_defs e_env h_env t_env (TInt, []) e2 ; (TBool, [])
    | Less (e1, e2) -> check eff_defs e_env h_env t_env (TInt, []) e1 ; check eff_defs e_env h_env t_env (TInt, []) e2 ; (TBool, [])
    | If (e1, e2, e3) ->
      check eff_defs e_env h_env t_env (TBool, []) e1 ;
      let ty = type_of eff_defs e_env h_env t_env e2 in
	      check eff_defs e_env h_env t_env ty e3 ; ty
    | FullFun (x, es1, hs, ts, t, es2, exp) -> (TUnit, [])
    | Assign (x, e) -> (TUnit, [])
    | Let (x, e1, e2) -> (TUnit, [])
    | Decl (x, e1, e2) -> (TUnit, [])
    | Handle (x, fname, e1, e2) -> (TUnit, [])
    (* | Fun (f, x, ty1, ty2, e) ->
      check eff_defs e_env h_env ((f, TArrow(ty1,ty2)) :: (x, ty1) :: t_env) ty2 e ;
      (TArrow (ty1, ty2), []) *)
    | Apply (exp1, exp2) ->
      begin match type_of eff_defs e_env h_env t_env exp1 with
	      TAbs (es1, hs, ts, t, es2), es3 -> 
          (* check eff_defs e_env h_env t_env (ty1, []) exp2 ; ty2 *)
          (TUnit, [])
      | ty, es1 ->
          typing_error ~loc
            "this expression is used as a function but its type is %t" (Print.ty ty)
      end
    | Seq (exp1, exp2) -> (TUnit, [])
