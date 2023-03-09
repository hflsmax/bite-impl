
open Syntax

let spf = Printf.sprintf

let full_fun_to_tabs (exp : expr') : ty =
  match exp with
  | FullFun (x, es1, hs, tm_args, ty, es2, exp) ->
    let tm_args_ty = List.split tm_args |> snd in
    TAbs (es1, hs, tm_args_ty, ty, es2)
  | _ -> failwith "full_fun_to_tabs: can only be called on FullFun"


let rec gather_locals ({Zoo.data=exp; loc} : expr) : locals =
  match exp with
  | Times (e1, e2) | Plus (e1, e2) | Minus (e1, e2)
  | Equal (e1, e2) | Less (e1, e2) ->
      gather_locals e1 @ gather_locals e2
  | Assign (x, e) -> gather_locals e
  | If (e1, e2, e3) ->
      gather_locals e1 @ gather_locals e2 @ gather_locals e3
  | Let (x, ty, e1, e2) ->
      (x, ty) :: gather_locals e1 @ gather_locals e2
  | Decl (x, ty, e1, e2) ->
      (x, TMut ty) :: gather_locals e1 @ gather_locals e2
  | Handle (x, (_, ty), catch_exp, handle_exp) ->
      (x, ty) :: gather_locals catch_exp @ gather_locals handle_exp
  | FullFun (x, _, _, _, _, _, _) ->
      (x, TStackClosure (full_fun_to_tabs exp)) :: []
  | FullApply ((e1, _), _, _, e2) -> 
      gather_locals e1 @ List.fold_left (fun acc exp_iter -> acc @ (gather_locals exp_iter)) [] e2
  | Raise (_, _, _, e) ->
      List.fold_left (fun acc exp_iter -> acc @ (gather_locals exp_iter)) [] e
  | Seq (e1, e2) -> gather_locals e1 @ gather_locals e2
  | Var _ | Int _ | Bool _ | Deref _  -> []

let rec ty_to_string ty : string =
  match ty with
  | TInt -> "int"
  | TBool -> "bool"
  | TMut ty -> ty_to_string ty
  | TAbs _ -> "closure_t*"
  | TStackClosure _ -> "closure_t"


    (* match ty' with
    | TAbs (es1, hs, ty_args, ty, es2) ->
      (* The string "PLACEHOLDER" takes the place of the variable name.
         It is not needed when the type is not used in a declaration, so
         we remove them.  *)
      let clean_format = fun s -> Str.global_replace (Str.regexp "PLACEHOLDER") "" s in
      (* The first parameter is the env pointer. *)
      let ty_args = "void*" :: List.map (fun ty_arg -> ty_to_string name ty_arg |> clean_format) ty_args in
      let handler_ty_args = List.map (fun (h, (_, fname_ty)) -> 
        [Str.global_replace (Str.regexp "PLACEHOLDER") "" (ty_to_string name fname_ty);
        "void*"]) hs 
        |> List.flatten
      in
      Printf.sprintf "%s (*PLACEHOLDER)(%s)" (ty_to_string name ty) (String.concat ", " (ty_args @ handler_ty_args)) *)

let get_fullfun_name (exp : expr') : string =
  match exp with
  | FullFun (x, es1, hs, tm_args, ty, es2, exp) -> x
  | _ -> failwith "get_fullfun_name: can only be called on FullFun"

let wrap_in_main (exp : expr) : expr =
  Zoo.locate (FullFun ("main", [], [], [], TInt, [], exp))