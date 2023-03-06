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
and type_of (eff_defs : f_ENV) (e_env : e_ENV) (h_env : h_ENV) (t_env : t_ENV) {Zoo.data=e; loc} : (ty * effs) =
  match e with
    | Var x ->
      (try (List.assoc x t_env, []) 
       with Not_found -> typing_error ~loc "unknown variable %s" x)
    | Int _ -> (TInt, [])
    | Bool _ -> (TBool, [])
    | Unit -> (TUnit, [])
    | Times (exp1, exp2) | Plus (exp1, exp2) | Minus (exp1, exp2) -> 
      let ty1, es1 = type_of eff_defs e_env h_env t_env exp1 in
      let ty2, es2 = type_of eff_defs e_env h_env t_env exp2 in
      if (ty1 <> TInt || ty2 <> TInt) then typing_error ~loc "This expression can't be type checked";
      (TInt, es1 @ es2)
    (* | Plus (exp1, exp2) -> check eff_defs e_env h_env t_env (TInt, []) exp1 ; check eff_defs e_env h_env t_env (TInt, []) exp2 ; (TInt, []) *)
    (* | Minus (exp1, exp2) -> check eff_defs e_env h_env t_env (TInt, []) exp1 ; check eff_defs e_env h_env t_env (TInt, []) exp2 ; (TInt, []) *)
    | Equal (exp1, exp2) | Less (exp1, exp2) ->
      let ty1, es1 = type_of eff_defs e_env h_env t_env exp1 in
      let ty2, es2 = type_of eff_defs e_env h_env t_env exp2 in
      if (ty1 <> TInt || ty2 <> TInt) then typing_error ~loc "This expression can't be type checked";
      (TBool, es1 @ es2)
    (* | Less (exp1, exp2) -> check eff_defs e_env h_env t_env (TInt, []) exp1 ; check eff_defs e_env h_env t_env (TInt, []) exp2 ; (TBool, []) *)
    | If (exp1, exp2, exp3) ->
      let ty1, es1 = type_of eff_defs e_env h_env t_env exp1 in
      let ty2, es2 = type_of eff_defs e_env h_env t_env exp2 in
      let ty3, es3 = type_of eff_defs e_env h_env t_env exp3 in
      if (ty1 <> TBool || ty2 <> ty3) then typing_error ~loc "This expression can't be type checked";
      (ty2, es1 @ es2 @ es3)
    | FullFun (x, es1, hs, ts, t, es2, exp) -> (TUnit, [])
    | Assign (x, exp) -> 
      let ty_e, es_e = type_of eff_defs e_env h_env t_env exp in
      (try 
        match List.assoc x t_env with
          | TMut ty_x -> if ty_x <> ty_e then typing_error ~loc "This expression can't be type checked";
            (ty_e, es_e)
          | _ -> typing_error ~loc "LHS of assignment must be of mutable type";
       with Not_found -> typing_error ~loc "unknown variable %s" x)
    | Let (x, exp1, exp2) ->
      let ty1, es1 = type_of eff_defs e_env h_env t_env exp1 in
      let ty2, es2 = type_of eff_defs e_env h_env ((x, ty1) :: t_env) exp2 in
      (ty2, es1 @ es2)
    | Decl (x, exp1, exp2) ->
      let ty1, es1 = type_of eff_defs e_env h_env t_env exp1 in
      let ty2, es2 = type_of eff_defs e_env h_env ((x, TMut ty1) :: t_env) exp2 in
      (ty2, es1 @ es2)
    | Handle (x, fname, exp_handle, exp_catch) ->
      let ty_handle, es_handle = type_of eff_defs e_env ((HVar x, fname) :: h_env) t_env exp_handle in
      let ty_catch, es_catch = type_of eff_defs e_env h_env t_env exp_catch in
      (try 
        match List.assoc fname eff_defs with
          | TAbs (es1, hs, ts, t, es2) as ty_fname -> 
            if ty_handle <> t then typing_error ~loc "The type of handle expression and catch body must be the same.";
            if ty_catch <> ty_fname then typing_error ~loc "The handler's type much match the type of effect definition.";
            (ty_handle, es_handle)
          | _ -> typing_error ~loc "effect definition must be of type TAbs";
       with Not_found -> typing_error ~loc "unknown effect name %s" fname)
    (* | Fun (f, x, ty1, ty2, e) ->
      check eff_defs e_env h_env ((f, TArrow(ty1,ty2)) :: (x, ty1) :: t_env) ty2 e ;
      (TArrow (ty1, ty2), []) *)
    (* | Apply (exp1, exp2) ->
      begin match type_of eff_defs e_env h_env t_env exp1 with
	      TAbs (es1, hs, ts, t, es2), es3 -> 
          (* check eff_defs e_env h_env t_env (ty1, []) exp2 ; ty2 *)
          (TUnit, [])
      | ty, es1 ->
          typing_error ~loc
            "this expression is used as a function but its type is %t" (Print.ty ty)
      end *)
    | FullApply (exp1, es, hs, exps) -> (TUnit, [])
    | Seq (exp1, exp2) ->
      let ty1, es1 = type_of eff_defs e_env h_env t_env exp1 in
      let ty2, es2 = type_of eff_defs e_env h_env t_env exp2 in
      (ty2, es1 @ es2)
      
