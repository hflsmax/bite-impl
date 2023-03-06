(** Type checking. *)

open Syntax

let typing_error ~loc = Zoo.error ~kind:"Type error" ~loc

let fname_ok (eff_defs : f_ENV) (fname : fname) =
  if not (List.mem_assoc fname eff_defs) then
    Zoo.error ~kind:"Type error" "unknown effect %s" fname

let hd_ok loc (h_env : h_ENV) (s : hvar) =
  if not (List.mem_assoc s h_env) then
    Zoo.error ~loc ~kind:"Type error" "unknown handler %s in %t" s (Print.h_ENV h_env)

let eff_ok (e_env : e_ENV) (h_env : h_ENV) (e : eff) =
    match e with
      EVar s -> if not (List.mem e e_env) then
        Zoo.error ~kind:"Type error" "unknown effect var %s" s
    | Handler s -> if not (List.mem_assoc s h_env) then
        Zoo.error ~kind:"Type error" "unknown handler var %s" s
    
let rec ty_ok (eff_defs : f_ENV) (e_env : e_ENV) (h_env : h_ENV) ty =
  match ty with
    | TAbs (es1, hs, ts, t, es2) ->
        List.iter (ty_ok eff_defs (es1 @ e_env) (hs @ h_env)) ts ;
        List.iter (fname_ok eff_defs) (snd (List.split hs)) ;
        ty_ok eff_defs (es1 @ e_env) (hs @ h_env) t ;
        List.iter (eff_ok (es1 @ e_env) (hs @ h_env)) es2
    | _ -> ()


let rec type_of (eff_defs : f_ENV) (e_env : e_ENV) (h_env : h_ENV) (t_env : t_ENV) {Zoo.data=e; loc} : (ty * effs) =
  match e with
    | Var x ->
      (try (List.assoc x t_env, []) 
       with Not_found -> typing_error ~loc "unknown variable %s" x)
    | Int _ -> (TInt, [])
    | Bool _ -> (TBool, [])
    | Times (exp1, exp2) | Plus (exp1, exp2) | Minus (exp1, exp2) -> 
      let ty1, es1 = type_of eff_defs e_env h_env t_env exp1 in
      let ty2, es2 = type_of eff_defs e_env h_env t_env exp2 in
      if (ty1 <> TInt || ty2 <> TInt) then typing_error ~loc "In a times exp: lhs type is %t, rhs type is %t" (Print.ty ty1) (Print.ty ty2);
      (TInt, es1 @ es2)
    | Equal (exp1, exp2) | Less (exp1, exp2) ->
      let ty1, es1 = type_of eff_defs e_env h_env t_env exp1 in
      let ty2, es2 = type_of eff_defs e_env h_env t_env exp2 in
      if (ty1 <> TInt || ty2 <> TInt) then typing_error ~loc "In an equal exp: lhs type is %t, rhs type is %t" (Print.ty ty1) (Print.ty ty2);
      (TBool, es1 @ es2)
    | If (exp1, exp2, exp3) ->
      let ty1, es1 = type_of eff_defs e_env h_env t_env exp1 in
      let ty2, es2 = type_of eff_defs e_env h_env t_env exp2 in
      let ty3, es3 = type_of eff_defs e_env h_env t_env exp3 in
      if (ty1 <> TBool || ty2 <> ty3) then typing_error ~loc "%t can't be type checked" (Print.expr e);
      (ty2, es1 @ es2 @ es3)
    | FullFun (x, es1, hs, tm_args, ty, es2, exp) ->
      let ty_args = List.map snd tm_args in
      List.iter (ty_ok eff_defs (es1 @ e_env) (hs @ h_env)) ty_args ;
      List.iter (fname_ok eff_defs) (snd (List.split hs)) ;
      ty_ok eff_defs (es1 @ e_env) (hs @ h_env) ty ;
      List.iter (eff_ok (es1 @ e_env) (hs @ h_env)) es2 ;
      let ty_e, es_e = type_of eff_defs (es1 @ e_env) (hs @ h_env) ((x, TAbs (es1, hs, ty_args, ty, es2)) :: tm_args @ t_env) exp in
      if ty_e <> ty then typing_error ~loc "The function body has the wrong type, expected %t but got %t" (Print.ty ty) (Print.ty ty_e);
      if not (List.for_all (fun e -> List.mem e es2) es_e) then typing_error ~loc "The function body has more effects than allowed by the function type";
      (TAbs (es1, hs, ty_args, ty, es2), [])
    | Assign (x, exp) -> 
      let ty_e, es_e = type_of eff_defs e_env h_env t_env exp in
      (try 
        match List.assoc x t_env with
          | TMut ty_x -> if ty_x <> ty_e then typing_error ~loc "This expression can't be type checked";
            (ty_e, es_e)
          | _ -> typing_error ~loc "LHS of assignment must be of mutable type";
       with Not_found -> typing_error ~loc "unknown variable %s" x)
    | Deref x ->
      (try 
        match List.assoc x t_env with
          | TMut ty_x -> (ty_x, [])
          | _ -> typing_error ~loc "Operand to deref must be of mutable type";
       with Not_found -> typing_error ~loc "unknown variable %s" x)
    | Let (x, exp1, exp2) ->
      let ty1, es1 = type_of eff_defs e_env h_env t_env exp1 in
      let ty2, es2 = type_of eff_defs e_env h_env ((x, ty1) :: t_env) exp2 in
      (ty2, es1 @ es2)
    | Decl (x, exp1, exp2) ->
      let ty1, es1 = type_of eff_defs e_env h_env t_env exp1 in
      let ty2, es2 = type_of eff_defs e_env h_env ((x, TMut ty1) :: t_env) exp2 in
      (ty2, es1 @ es2)
    | Handle (x, fname, exp_catch, exp_handle) ->
      let ty_handle, es_handle = type_of eff_defs e_env ((x, fname) :: h_env) t_env exp_handle in
      let ty_catch, es_catch = type_of eff_defs e_env h_env t_env exp_catch in
      (try 
        match List.assoc fname eff_defs with
          | TAbs (es1, hs, ts, t, es2) as ty_fname -> 
            if ty_handle <> t then typing_error ~loc "The type of handle expression and catch body must be the same.";
            if ty_catch <> ty_fname then typing_error ~loc "The handler's type must match the type of effect definition, expected %t but got %t" (Print.ty ty_fname) (Print.ty ty_catch);
            (ty_handle, es_handle)
          | _ -> typing_error ~loc "effect definition must be of type TAbs";
       with Not_found -> typing_error ~loc "unknown effect name %s" fname)
    | FullApply (exp1, es, hs, exps) ->
      let ty1, es1 = type_of eff_defs e_env h_env t_env exp1 in
      (match ty1 with
      | TAbs (es1', hs', ts', t', es2') -> 
        List.iter (eff_ok e_env h_env) es;
        List.iter (hd_ok loc h_env) hs;
        let tys, ess = List.split (List.map (type_of eff_defs e_env h_env t_env) exps) in
        if List.length es <> List.length es1' then typing_error ~loc "Wrong number of effect arguments.";
        let actual_fnames = List.map (fun h -> List.assoc h h_env) hs in
          let expected_fnames = snd (List.split hs') in
          if actual_fnames <> expected_fnames then typing_error ~loc "Wrong handlers types, expected %s, got %s" (String.concat ", " expected_fnames) (String.concat ", " actual_fnames);
        if tys <> ts' then typing_error ~loc "Wrong types of term arguments.";
        (t', es1 @ List.concat ess)
      | _ -> typing_error ~loc "The lhs of application must be of type TAbs")
    | Raise (hvar, es, hs, exps) ->
      List.assoc_opt hvar h_env |> (function
        | Some fname -> 
          (try 
            match List.assoc fname eff_defs with
              | TAbs (es1', hs', ts', t', es2') -> 
                List.iter (eff_ok e_env h_env) es;
                List.iter (hd_ok loc h_env) hs;
                let tys, ess = List.split (List.map (type_of eff_defs e_env h_env t_env) exps) in
                if List.length es <> List.length es1' then typing_error ~loc "Wrong number of effect arguments.";
                if (List.map (fun h -> List.assoc h h_env) hs) <> snd (List.split hs') then typing_error ~loc "Wrong types of handler arguments.";
                if tys <> ts' then typing_error ~loc "Wrong types of term arguments.";
                (t', List.concat ess)
              | _ -> typing_error ~loc "effect definition must be of type TAbs";
           with Not_found -> typing_error ~loc "unknown effect name %s" fname)
        | None -> typing_error ~loc "unknown handler %s" hvar)
    | Seq (exp1, exp2) ->
      let ty1, es1 = type_of eff_defs e_env h_env t_env exp1 in
      let ty2, es2 = type_of eff_defs e_env h_env t_env exp2 in
      (ty2, es1 @ es2)
      
