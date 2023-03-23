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
    | HVar s -> if not (List.mem_assoc s h_env) then
        Zoo.error ~kind:"Type error" "unknown handler var %s" s
    
let rec ty_ok (eff_defs : f_ENV) (e_env : e_ENV) (h_env : h_ENV) ty =
  match ty with
    | TAbs (es1, hs, ts, t, es2) ->
        let new_e_env = es1 @ e_env in
        let new_h_env = List.map (fun (x, fname) -> (x, fname)) hs @ h_env in
        List.iter (ty_ok eff_defs new_e_env new_h_env) ts ;
        List.iter (fname_ok eff_defs) (snd (List.split hs));
        ty_ok eff_defs new_e_env new_h_env t ;
        List.iter (eff_ok new_e_env new_h_env) es2
    | _ -> ()

let hvar_to_rich_hvar eff_defs h_env h = let fname = List.assoc h h_env in h, fname, List.assoc fname eff_defs

let default_attrs = Syntax.R.default_attrs

let type_of_buildin = function
  | ArrayInit -> TAbs ([], [], [TInt], TBuildIn, [])
  | ArrayGet -> TAbs ([], [], [TBuildIn; TInt], TInt, [])

let fn_index = ref 0
let rec type_of (eff_defs : f_ENV) (e_env : e_ENV) (h_env : h_ENV) (t_env : t_ENV) {Zoo.data=e; loc} : R.expr =
  match e with
    | Var x ->
      begin
      match List.assoc_opt x buildin_fun with
      | None -> (try (R.Var(0, x), List.assoc x t_env, [], default_attrs) 
                with Not_found -> typing_error ~loc "unknown variable %s" x)
      | Some b -> (R.Var(0, x), type_of_buildin b, [], {default_attrs with isBuildIn = true})
      end
    | Int i -> (R.Int i, TInt, [], default_attrs)
    | Bool b -> (R.Bool b, TBool, [], default_attrs)
    | Times (exp1, exp2) ->
      let exp1', ty1, es1, attrs1 = type_of eff_defs e_env h_env t_env exp1 in
      let exp2', ty2, es2, attrs2 = type_of eff_defs e_env h_env t_env exp2 in
      if (ty1 <> TInt || ty2 <> TInt) then typing_error ~loc "In a times exp: lhs type is %t, rhs type is %t" (Print.ty ty1) (Print.ty ty2);
      Times ((exp1', ty1, es1, attrs1), (exp2', ty2, es2, attrs2)), TInt, es1 @ es2, default_attrs
    | Plus (exp1, exp2) ->
      let exp1', ty1, es1, attrs1 = type_of eff_defs e_env h_env t_env exp1 in
      let exp2', ty2, es2, attrs2 = type_of eff_defs e_env h_env t_env exp2 in
      if (ty1 <> TInt || ty2 <> TInt) then typing_error ~loc "In a plus exp: lhs type is %t, rhs type is %t" (Print.ty ty1) (Print.ty ty2);
      Plus ((exp1', ty1, es1, attrs1), (exp2', ty2, es2, attrs2)), TInt, es1 @ es2, default_attrs
    | Minus (exp1, exp2) ->
      let exp1', ty1, es1, attrs1 = type_of eff_defs e_env h_env t_env exp1 in
      let exp2', ty2, es2, attrs2 = type_of eff_defs e_env h_env t_env exp2 in
      if (ty1 <> TInt || ty2 <> TInt) then typing_error ~loc "In a minus exp: lhs type is %t, rhs type is %t" (Print.ty ty1) (Print.ty ty2);
      Minus ((exp1', ty1, es1, attrs1), (exp2', ty2, es2, attrs2)), TInt, es1 @ es2, default_attrs
    | Equal (exp1, exp2) ->
      let exp1', ty1, es1, attrs1 = type_of eff_defs e_env h_env t_env exp1 in
      let exp2', ty2, es2, attrs2 = type_of eff_defs e_env h_env t_env exp2 in
      if (ty1 <> TInt || ty2 <> TInt) then typing_error ~loc "In an equal exp: lhs type is %t, rhs type is %t" (Print.ty ty1) (Print.ty ty2);
      Equal ((exp1', ty1, es1, attrs1), (exp2', ty2, es2, attrs2)), TBool, es1 @ es2, default_attrs
    | Less (exp1, exp2) ->
      let exp1', ty1, es1, attrs1 = type_of eff_defs e_env h_env t_env exp1 in
      let exp2', ty2, es2, attrs2 = type_of eff_defs e_env h_env t_env exp2 in
      if (ty1 <> TInt || ty2 <> TInt) then typing_error ~loc "In an equal exp: lhs type is %t, rhs type is %t" (Print.ty ty1) (Print.ty ty2);
      Less ((exp1', ty1, es1, attrs1), (exp2', ty2, es2, attrs2)), TBool, es1 @ es2, default_attrs
    | If (exp1, exp2, exp3) ->
      let exp1', ty1, es1, attrs1 = type_of eff_defs e_env h_env t_env exp1 in
      let exp2', ty2, es2, attrs2 = type_of eff_defs e_env h_env t_env exp2 in
      let exp3', ty3, es3, attrs3 = type_of eff_defs e_env h_env t_env exp3 in
      if (ty1 <> TBool) then typing_error ~loc "In an if exp: condition type is %t" (Print.ty ty1);
      if (ty2 <> ty3) then typing_error ~loc "%t and %t are not the same type" (Print.ty ty2) (Print.ty ty3);
      If ((exp1', ty1, es1, attrs1), (exp2', ty2, es2, attrs2), (exp3', ty3, es3, attrs3)), ty2, es1 @ es2 @ es3, default_attrs
    | FullFun (kind, x, es1, hs, tm_args, ty, es2, exp_body) ->
      let x = if x = None then (fn_index := !fn_index + 1; "fn" ^ string_of_int !fn_index) else Option.get x in
      let ty_args = List.map snd tm_args in
      let new_e_env = es1 @ e_env in
      let new_h_env = hs @ h_env in
      List.iter (ty_ok eff_defs new_e_env new_h_env) ty_args ;
      List.iter (fname_ok eff_defs) (snd (List.split hs)) ;
      ty_ok eff_defs new_e_env new_h_env ty ;
      List.iter (eff_ok new_e_env new_h_env) es2 ;
      let exp_body', ty_e, es_e, attrs_e = type_of eff_defs new_e_env new_h_env ((x, TAbs (es1, hs, ty_args, ty, es2)) :: tm_args @ t_env) exp_body in
        if kind = Lambda && ty_e <> ty then typing_error ~loc "The function body has the wrong type, expected %t but got %t" (Print.ty ty) (Print.ty ty_e);
        if kind = Lambda && (List.exists (fun e -> not (List.mem e es2)) es_e) then typing_error ~loc "The function body has more effects than allowed by the function type";
      let hs' = List.map (fun (name, fname) -> (name, fname, List.assoc fname eff_defs)) hs in
      FullFun (kind, x, es1, hs', tm_args, ty, es2, (exp_body', ty_e, es_e, attrs_e)), TAbs (es1, hs, ty_args, ty, es2), [], default_attrs
    | Assign (v_exp, exp) -> 
      begin match v_exp.data with
        | Var x ->
          let v_exp', ty_v, es_v, attrs_v = type_of eff_defs e_env h_env t_env v_exp in
          let exp', ty_e, es_e, attrs_e = type_of eff_defs e_env h_env t_env exp in
          (try 
            match List.assoc x t_env with
              | TMut ty_x -> if ty_x <> ty_e then typing_error ~loc "This expression can't be type checked";
              Assign ((v_exp', ty_v, es_v, attrs_v), (exp', ty_e, es_e, attrs_e)), ty_e, es_e, default_attrs
              | _ -> typing_error ~loc "LHS of assignment must be of mutable type";
          with Not_found -> typing_error ~loc "unknown variable %s" x)
        | _ -> typing_error ~loc "LHS of assignment must be a variable"
      end
    | Deref v_exp ->
      begin match v_exp.data with
        | Var x ->
          (try 
            match List.assoc x t_env with
              | TMut ty_x -> 
                let v_exp', ty_v, es_v, attrs_v = type_of eff_defs e_env h_env t_env v_exp in 
                  Deref (v_exp', ty_v, es_v, attrs_v), ty_x, [], default_attrs
              | _ -> typing_error ~loc "Operand to deref must be of mutable type";
          with Not_found -> typing_error ~loc "unknown variable %s" x)
        | _ -> typing_error ~loc "Operand to deref must be a variable"
      end
    | Let (x, exp1, exp2) ->
      let exp1', ty1, es1, attrs1 = type_of eff_defs e_env h_env t_env exp1 in
      let exp2', ty2, es2, attrs2 = type_of eff_defs e_env h_env ((x, ty1) :: t_env) exp2 in
      Let (x, ty1, (exp1', ty1, es1, attrs1), (exp2', ty2, es2, attrs2)), ty2, es1 @ es2, default_attrs
    | Decl (x, exp1, exp2) ->
      let exp1', ty1, es1, attrs1 = type_of eff_defs e_env h_env t_env exp1 in
      let exp2', ty2, es2, attrs2 = type_of eff_defs e_env h_env ((x, TMut ty1) :: t_env) exp2 in
      Decl ((x, ty1, (exp1', ty1, es1, attrs1), (exp2', ty2, es2, attrs2))), ty2, es1 @ es2, default_attrs
    | Handle (x, fname, exp_catch, exp_handle) ->
      let exp_catch', ty_catch, es_catch, attrs_catch = type_of eff_defs e_env h_env t_env exp_catch in
      let exp_handle', ty_handle, es_handle, attrs_handle = type_of eff_defs e_env ((x, fname) :: h_env) t_env exp_handle in
      (try 
        match List.assoc fname eff_defs with
          | TAbs (es1, hs, ts, t, es2) as ty_fname -> 
            (* TODO: check that the answer type matches *)
            (* if ty_handle <> t then typing_error ~loc "The type of handle expression and catch body must be the same."; *)
            if ty_catch <> ty_fname then typing_error ~loc "The handler's type must match the type of effect definition, expected %t but got %t" (Print.ty ty_fname) (Print.ty ty_catch);
            Handle (x, (fname, ty_fname), (exp_catch', ty_catch, es_catch, attrs_catch), (exp_handle', ty_handle, es_handle, attrs_handle)), ty_handle, List.filter (fun e -> e <> HVar x) es_handle, default_attrs
          | _ -> typing_error ~loc "effect definition must be of type TAbs";
       with Not_found -> typing_error ~loc "unknown effect name %s" fname)
    | FullApply (exp1, es, hs, exps) ->
      let exp1', ty1, es1, attrs1 = type_of eff_defs e_env h_env t_env exp1 in
      (match ty1 with
      | TAbs (es1', hs', ts', t', es2') -> 
        List.iter (eff_ok e_env h_env) es;
        List.iter (hd_ok loc h_env) hs;
        let exps_list = List.map (type_of eff_defs e_env h_env t_env) exps in
        let exps', tys, ess = List.fold_right (fun (a,b,c,_) (s1,s2,s3) -> a::s1,b::s2,c::s3) exps_list ([],[],[]) in
        if List.length es <> List.length es1' then typing_error ~loc "Wrong number of effect arguments.";
        let actual_fnames = List.map (fun h -> List.assoc h h_env) hs in
        let expected_fnames = snd (List.split hs') in
          if actual_fnames <> expected_fnames then typing_error ~loc "Wrong handlers types, expected %s, got %s" (String.concat ", " expected_fnames) (String.concat ", " actual_fnames);
        if tys <> ts' then typing_error ~loc "Wrong types of term arguments, expected %t, got %t" (Print.tys ts') (Print.tys tys);
        let hs'' = List.map (hvar_to_rich_hvar eff_defs h_env) hs in
        FullApply ((exp1', ty1, es1, attrs1), es, hs'', exps_list), t', es2' @ es1 @ List.concat ess, default_attrs
      | _ -> typing_error ~loc "The lhs of application must be of type TAbs")
    | Raise (hvar, es, hs, exps) ->
      List.assoc_opt hvar h_env |> (function
        | Some fname -> 
          (try 
            match List.assoc fname eff_defs with
              | TAbs (es1', hs', ts', t', es2') -> 
                List.iter (eff_ok e_env h_env) es;
                List.iter (hd_ok loc h_env) hs;
                let exps_list = List.map (type_of eff_defs e_env h_env t_env) exps in
                let exps', tys, ess = List.fold_right (fun (a,b,c,_) (s1,s2,s3) -> a::s1,b::s2,c::s3) exps_list ([],[],[]) in
                if List.length es <> List.length es1' then typing_error ~loc "Wrong number of effect arguments.";
                if (List.map (fun h -> List.assoc h h_env) hs) <> snd (List.split hs') then typing_error ~loc "Wrong types of handler arguments.";
                if tys <> ts' then typing_error ~loc "Wrong types of term arguments.";
                let hvar' = hvar_to_rich_hvar eff_defs h_env hvar in
                let hs'' = List.map (hvar_to_rich_hvar eff_defs h_env) hs in
                R.Raise (hvar', es, hs'', exps_list), t', HVar hvar :: es2' @ List.concat ess, default_attrs
              | _ -> typing_error ~loc "effect definition must be of type TAbs";
           with Not_found -> typing_error ~loc "unknown effect name %s" fname)
        | None -> typing_error ~loc "unknown handler %s" hvar)
    | Resume e ->
      (* TODO: check e's type corresponds to handler's return type *)
      let e', ty_e, es_e, attrs_e = type_of eff_defs e_env h_env t_env e in
        Resume (e', ty_e, es_e, attrs_e), ty_e, es_e, default_attrs
    | Seq (exp1, exp2) ->
      let exp1', ty1, es1, attrs1 = type_of eff_defs e_env h_env t_env exp1 in
      let exp2', ty2, es2, attrs2 = type_of eff_defs e_env h_env t_env exp2 in
      Seq ((exp1', ty1, es1, attrs1), (exp2', ty2, es2, attrs2)), ty2, es1 @ es2, default_attrs