(** Type checking. *)

open Syntax
open Util

let typing_error ~loc = error ~kind:"Type error" ~loc

let fname_ok (eff_defs : f_ENV) (fname : fname) =
  if not (List.mem_assoc fname eff_defs) then
    error ~kind:"Type error" "unknown effect %s" fname

let hd_ok (h_env : h_ENV) (s : hvar) =
  if not (List.mem_assoc s h_env) then
    error ~kind:"Type error" "unknown handler %s in %t" s (Print.h_ENV h_env)

let eff_ok (e_env : e_ENV) (h_env : h_ENV) (e : eff) =
  match e with
  | EVar s ->
      if not (List.mem e e_env) then
        error ~kind:"Type error" "unknown effect var %s" s
  | HVar s ->
      if not (List.mem_assoc s h_env) then
        error ~kind:"Type error" "unknown handler var %s" s

let rec ty_ok (eff_defs : f_ENV) (e_env : e_ENV) (h_env : h_ENV) ty =
  match ty with
  | TAbs (es1, hs, ts, t, es2) ->
      let new_e_env = es1 @ e_env in
      let new_h_env = List.map (fun (x, fname) -> (x, fname)) hs @ h_env in
      List.iter (ty_ok eff_defs new_e_env new_h_env) ts;
      List.iter (fname_ok eff_defs) (snd (List.split hs));
      ty_ok eff_defs new_e_env new_h_env t;
      List.iter (eff_ok new_e_env new_h_env) es2
  | _ -> ()

let hvar_to_rich_hvar eff_defs h_env name =
  let fname = List.assoc name h_env in
  let ty = List.assoc fname eff_defs in
  { name; fname; ty; depth = -1 }

let default_attrs = Syntax.default_attrs
let fn_index = ref 0

let rec type_of (eff_defs : f_ENV) (e_env : e_ENV) (h_env : h_ENV)
    (t_env : t_ENV) (e, attrs) : expr =
  match e with
  | Var x -> (
      match List.assoc_opt x builtin_fun with
      | None -> (
          try (Var x, { default_attrs with ty = List.assoc x t_env })
          with Not_found ->
            typing_error ~loc:attrs.loc "unknown variable %s" x)
      | Some ty ->
          (Var x, { default_attrs with ty; effs = []; isBuiltin = true }))
  | Int i -> (Int i, { default_attrs with ty = TInt; effs = [] })
  | Bool b -> (Bool b, { default_attrs with ty = TBool; effs = [] })
  | Unit -> (Unit, { default_attrs with ty = TUnit; effs = [] })
  | AOP (op, exp1, exp2) ->
      let exp1', attrs1 = type_of eff_defs e_env h_env t_env exp1 in
      let exp2', attrs2 = type_of eff_defs e_env h_env t_env exp2 in
      if attrs1.ty <> TInt || attrs2.ty <> TInt then
        typing_error ~loc:attrs.loc
          "In an aop exp: lhs type is %t, rhs type is %t" (Print.ty attrs1.ty)
          (Print.ty attrs2.ty);
      ( AOP (op, (exp1', attrs1), (exp2', attrs2)),
        { default_attrs with ty = TInt; effs = attrs1.effs @ attrs2.effs } )
  | BOP (op, exp1, exp2) ->
      let exp1', attrs1 = type_of eff_defs e_env h_env t_env exp1 in
      let exp2', attrs2 = type_of eff_defs e_env h_env t_env exp2 in
      if attrs1.ty <> TInt || attrs2.ty <> TInt then
        typing_error ~loc:attrs.loc
          "In a bop exp: lhs type is %t, rhs type is %t" (Print.ty attrs1.ty)
          (Print.ty attrs2.ty);
      ( BOP (op, (exp1', attrs1), (exp2', attrs2)),
        { default_attrs with ty = TBool; effs = attrs1.effs @ attrs2.effs } )
  | If (exp1, exp2, exp3) ->
      let exp1', attrs1 = type_of eff_defs e_env h_env t_env exp1 in
      let exp2', attrs2 = type_of eff_defs e_env h_env t_env exp2 in
      let exp3', attrs3 = type_of eff_defs e_env h_env t_env exp3 in
      if attrs1.ty <> TBool then
        typing_error ~loc:attrs.loc "In an if exp: condition type is %t"
          (Print.ty attrs1.ty);
      if attrs2.ty <> attrs3.ty then
        typing_error ~loc:attrs.loc "%t and %t are not the same type"
          (Print.ty attrs2.ty) (Print.ty attrs3.ty);
      ( If ((exp1', attrs1), (exp2', attrs2), (exp3', attrs3)),
        {
          default_attrs with
          ty = attrs2.ty;
          effs = attrs1.effs @ attrs2.effs @ attrs2.effs;
        } )
  | FullFun (x, es1, hs, tm_args, ty, es2, exp_body) ->
      let x =
        if x = "" then (
          fn_index := !fn_index + 1;
          "fn" ^ string_of_int !fn_index)
        else x
      in
      let ty_args = List.map snd tm_args in
      let new_e_env = es1 @ e_env in
      let new_h_env = hs @ h_env in
      List.iter (ty_ok eff_defs new_e_env new_h_env) ty_args;
      List.iter (fname_ok eff_defs) (snd (List.split hs));
      ty_ok eff_defs new_e_env new_h_env ty;
      List.iter (eff_ok new_e_env new_h_env) es2;
      let exp_body', attrs_body =
        type_of eff_defs new_e_env new_h_env
          (((x, TAbs (es1, hs, ty_args, ty, es2)) :: tm_args) @ t_env)
          exp_body
      in
      if attrs_body.ty <> ty then
        typing_error ~loc:attrs.loc
          "The function body has the wrong type, expected %t but got %t"
          (Print.ty ty) (Print.ty attrs_body.ty);
      if List.exists (fun e -> not (List.mem e es2)) attrs_body.effs then
        typing_error ~loc:attrs.loc
          "The function body has more effects than allowed by the function type";
      ( FullFun (x, es1, hs, tm_args, ty, es2, (exp_body', attrs_body)),
        {
          default_attrs with
          ty = TAbs (es1, hs, ty_args, ty, es2);
          effs = [];
          hvarParams =
            Some
              (List.map
                 (fun (name, fname) ->
                   { name; fname; ty = List.assoc fname eff_defs; depth = -1 })
                 hs);
        } )
  | Assign (exp_v, exp) -> (
      match fst exp_v with
      | Var x -> (
          let exp_v', attrs_v = type_of eff_defs e_env h_env t_env exp_v in
          let exp', attrs_e = type_of eff_defs e_env h_env t_env exp in
          try
            match List.assoc x t_env with
            | TMut ty_x ->
                if ty_x <> attrs_e.ty then
                  typing_error ~loc:attrs.loc
                    "This expression can't be type checked";
                ( Assign ((exp_v', attrs_v), (exp', attrs_e)),
                  { default_attrs with ty = attrs_e.ty; effs = attrs_e.effs } )
            | _ ->
                typing_error ~loc:attrs.loc
                  "LHS of assignment must be of mutable type"
          with Not_found ->
            typing_error ~loc:attrs.loc "unknown variable %s" x)
      | _ -> typing_error ~loc:attrs.loc "LHS of assignment must be a variable")
  | Deref v_exp -> (
      match fst v_exp with
      | Var x -> (
          try
            match List.assoc x t_env with
            | TMut ty_x ->
                let v_exp', attrs_v =
                  type_of eff_defs e_env h_env t_env v_exp
                in
                ( Deref (v_exp', attrs_v),
                  { default_attrs with ty = ty_x; effs = [] } )
            | _ ->
                typing_error ~loc:attrs.loc
                  "Operand to deref must be of mutable type"
          with Not_found ->
            typing_error ~loc:attrs.loc "unknown variable %s" x)
      | _ -> typing_error ~loc:attrs.loc "Operand to deref must be a variable")
  | Let (x, isTop, exp1, exp2) ->
      let exp1', attrs1 = type_of eff_defs e_env h_env t_env exp1 in
      let exp2', attrs2 =
        type_of eff_defs e_env h_env ((x, attrs1.ty) :: t_env) exp2
      in
      ( Let (x, isTop, (exp1', attrs1), (exp2', attrs2)),
        { default_attrs with ty = attrs2.ty; effs = attrs1.effs @ attrs2.effs }
      )
  | Decl (x, isTop, exp1, exp2) ->
      let exp1', attrs1 = type_of eff_defs e_env h_env t_env exp1 in
      let exp2', attrs2 =
        type_of eff_defs e_env h_env ((x, TMut attrs1.ty) :: t_env) exp2
      in
      ( Decl (x, isTop, (exp1', attrs1), (exp2', attrs2)),
        { default_attrs with ty = attrs1.ty; effs = attrs1.effs @ attrs2.effs }
      )
  | Handle (x, fname, exp_catch, exp_handle) -> (
      let exp_catch', attrs_catch =
        type_of eff_defs e_env h_env t_env exp_catch
      in
      let exp_handle', attrs_handle =
        type_of eff_defs e_env ((x, fname) :: h_env) t_env exp_handle
      in
      try
        match List.assoc fname eff_defs with
        | TAbs (es1, hs, ts, t, es2) as ty_fname ->
            (* TODO: check that the answer type matches *)
            if attrs_catch.ty <> ty_fname then
              typing_error ~loc:attrs.loc
                "The handler's type must match the type of effect definition, \
                 expected %t but got %t"
                (Print.ty ty_fname) (Print.ty attrs_catch.ty);
            ( Handle
                ( x,
                  fname,
                  (exp_catch', attrs_catch),
                  (exp_handle', attrs_handle) ),
              {
                default_attrs with
                ty = attrs_handle.ty;
                effs = List.filter (fun e -> e <> HVar x) attrs_handle.effs;
                bindHvar = Some { name = x; fname; ty = ty_fname; depth = -1 };
              } )
        | _ ->
            typing_error ~loc:attrs.loc "effect definition must be of type TAbs"
      with Not_found ->
        typing_error ~loc:attrs.loc "unknown effect name %s" fname)
  | FullApply (exp1, es, hs, exps) -> (
      let exp1', attrs1 = type_of eff_defs e_env h_env t_env exp1 in
      match attrs1.ty with
      | TAbs (es1', hs', ts', t', es2') ->
          List.iter (eff_ok e_env h_env) es;
          List.iter (hd_ok h_env) hs;
          let exps_list = List.map (type_of eff_defs e_env h_env t_env) exps in
          let exps', tys, ess =
            List.fold_right
              (fun (exp, attrs) (s1, s2, s3) ->
                (exp :: s1, attrs.ty :: s2, attrs.effs :: s3))
              exps_list ([], [], [])
          in
          if List.length es <> List.length es1' then
            typing_error ~loc:attrs.loc "Wrong number of effect arguments.";
          let actual_fnames = List.map (fun h -> List.assoc h h_env) hs in
          let expected_fnames = snd (List.split hs') in
          if actual_fnames <> expected_fnames then
            typing_error ~loc:attrs.loc
              "Wrong handlers types, expected %s, got %s"
              (String.concat ", " expected_fnames)
              (String.concat ", " actual_fnames);
          if tys <> ts' then
            typing_error ~loc:attrs.loc
              "Wrong types of term arguments, expected %t, got %t"
              (Print.tys ts') (Print.tys tys);
          ( FullApply ((exp1', attrs1), es, hs, exps_list),
            {
              default_attrs with
              ty = t';
              effs = es2' @ attrs1.effs @ List.concat ess;
              hvarArgs = Some (List.map (hvar_to_rich_hvar eff_defs h_env) hs);
            } )
      | TBuiltin ->
          let exps_list = List.map (type_of eff_defs e_env h_env t_env) exps in
          ( FullApply ((exp1', attrs1), es, hs, exps_list),
            { default_attrs with ty = TBuiltin } )
      | _ ->
          typing_error ~loc:attrs.loc
            "The lhs of application \"%t\" of type \"%t\" must be of type TAbs"
            (Print.expr (fst exp1))
            (Print.ty attrs1.ty))
  | Raise (hvar, es, hs, exps) -> (
      List.assoc_opt hvar h_env |> function
      | Some fname -> (
          try
            match List.assoc fname eff_defs with
            | TAbs (es1', hs', ts', t', es2') ->
                List.iter (eff_ok e_env h_env) es;
                List.iter (hd_ok h_env) hs;
                let exps_list =
                  List.map (type_of eff_defs e_env h_env t_env) exps
                in
                let exps', tys, ess =
                  List.fold_right
                    (fun (exp, attrs) (s1, s2, s3) ->
                      (exp :: s1, attrs.ty :: s2, attrs.effs :: s3))
                    exps_list ([], [], [])
                in
                if List.length es <> List.length es1' then
                  typing_error ~loc:attrs.loc
                    "Wrong number of effect arguments.";
                if
                  List.map (fun h -> List.assoc h h_env) hs
                  <> snd (List.split hs')
                then
                  typing_error ~loc:attrs.loc
                    "Wrong types of handler arguments.";
                if tys <> ts' then
                  typing_error ~loc:attrs.loc "Wrong types of term arguments.";
                ( Raise (hvar, es, hs, exps_list),
                  {
                    default_attrs with
                    ty = t';
                    effs = (HVar hvar :: es2') @ List.concat ess;
                    hvarArgs =
                      Some (List.map (hvar_to_rich_hvar eff_defs h_env) hs);
                    lhsHvar = Some (hvar_to_rich_hvar eff_defs h_env hvar);
                  } )
            | _ ->
                typing_error ~loc:attrs.loc
                  "effect definition must be of type TAbs"
          with Not_found ->
            typing_error ~loc:attrs.loc "unknown effect name %s" fname)
      | None -> typing_error ~loc:attrs.loc "unknown handler %s" hvar)
  | Resume (e, r) ->
      (* TODO: check e's type corresponds to handler's return type *)
      let e', attrs_e = type_of eff_defs e_env h_env t_env e in
      ( Resume ((e', attrs_e), r),
        { default_attrs with ty = attrs_e.ty; effs = attrs_e.effs } )
  | Seq (exp1, exp2) ->
      let exp1', attrs1 = type_of eff_defs e_env h_env t_env exp1 in
      let exp2', attrs2 = type_of eff_defs e_env h_env t_env exp2 in
      ( Seq ((exp1', attrs1), (exp2', attrs2)),
        { default_attrs with ty = attrs2.ty; effs = attrs1.effs @ attrs2.effs }
      )
