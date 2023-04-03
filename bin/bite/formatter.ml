open Syntax

let rec expr_to_string ?(indent = 0) (e : expr) : string =
  let e', attrs = e in
  let attrs_str = format_attrs attrs in
  let indent_str = String.make (2 * indent) ' ' in
  let expr_str =
    match e' with
    | Var n -> "Var " ^ n
    | Int i -> "Int " ^ string_of_int i
    | Bool b -> "Bool " ^ string_of_bool b
    | Unit -> "Unit"
    | AOP (op, e1, e2) ->
        "AOP " ^ op ^ "  "
        ^ expr_to_string ~indent:(indent + 1) e1
        ^ "  "
        ^ expr_to_string ~indent:(indent + 1) e2
    | BOP (op, e1, e2) ->
        "BOP " ^ op ^ "  "
        ^ expr_to_string ~indent:(indent + 1) e1
        ^ "  "
        ^ expr_to_string ~indent:(indent + 1) e2
    | Assign (e1, e2) ->
        "Assign\n" ^ indent_str ^ "  "
        ^ expr_to_string ~indent:(indent + 1) e1
        ^ "\n" ^ indent_str ^ "  "
        ^ expr_to_string ~indent:(indent + 1) e2
    | Deref e -> "Deref " ^ expr_to_string ~indent:(indent + 1) e
    | If (e1, e2, e3) ->
        "If "
        ^ expr_to_string ~indent:(indent + 1) e1
        ^ "\n" ^ indent_str ^ "Then\n" ^ indent_str ^ "  "
        ^ expr_to_string ~indent:(indent + 1) e2
        ^ "\n" ^ indent_str ^ "Else\n" ^ indent_str ^ "  "
        ^ expr_to_string ~indent:(indent + 1) e3
    | Let (n, b, e1, e2) ->
        "Let " ^ n ^ " = "
        ^ (if b then "^ " else "")
        ^ expr_to_string ~indent:(indent + 1) e1
        ^ "\n" ^ indent_str ^ "in\n" ^ indent_str ^ "  "
        ^ expr_to_string ~indent:(indent + 1) e2
    | Decl (n, b, e1, e2) ->
        "Decl " ^ n ^ " = "
        ^ (if b then "^ " else "")
        ^ expr_to_string ~indent:(indent + 1) e1
        ^ "\n" ^ indent_str ^ "in\n" ^ indent_str ^ "  "
        ^ expr_to_string ~indent:(indent + 1) e2
    | Handle (n, f, e1, e2) ->
        "Handle " ^ n ^ " with " ^ f ^ "\n" ^ indent_str ^ "  "
        ^ expr_to_string ~indent:(indent + 1) e1
        ^ "\n" ^ indent_str ^ "in\n" ^ indent_str ^ "  "
        ^ expr_to_string ~indent:(indent + 1) e2
    | FullFun (n, effs', hvars, tys, t, effs'', e) ->
        let hvars_str =
          String.concat ", "
            (List.map (fun (hv, fn) -> "(" ^ hv ^ ", " ^ fn ^ ")") hvars)
        in
        let tys_str = String.concat ", " (List.map (fun (n, _) -> n) tys) in
           (* let effs_str = String.concat ", " effs' in
           let effs_str2 = String.concat ", " effs'' in *)
        "FullFun " ^ n ^ "\n" ^
        (* indent_str ^ "  Handlers: [" ^ hvars_str ^ "]\n" ^ *)
           indent_str ^ "  Args: [" ^ tys_str ^ "]\n"
           (* indent_str ^ "  Return: " ^ t ^ "\n" ^ *)
        ^ indent_str ^ "    "
        ^ expr_to_string ~indent:(indent + 2) e
    | FullApply (e, effs', hvars, es) ->
        (* let hvars_str = String.concat ", " hvars in *)
        let es_str =
          String.concat ", " (List.map (expr_to_string ~indent:(indent + 1)) es)
        in
        (* let effs_str = String.concat ", " effs' in *)
        "FullApply\n" ^ indent_str ^ "  LHS: "
        ^ expr_to_string ~indent:(indent + 1) e
        ^ "\n"
        (* indent_str ^ "  Effs: [" ^ effs_str ^ "]\n" ^ *)
        (* indent_str ^ "  Handlers: [" ^ hvars_str ^ "]\n" ^ *)
        ^ indent_str
        ^ "  Args: [" ^ es_str ^ "]"
    | Raise (hv, effs', hvars, es) ->
        (* let hvars_str = String.concat ", " hvars in *)
        let es_str =
          String.concat ", " (List.map (expr_to_string ~indent:(indent + 1)) es)
        in
        (* let effs_str = String.concat ", " effs' in *)
        "Raise " ^ hv ^ "\n"
        (* indent_str ^ "  Effs: [" ^ effs_str ^ "]\n" ^
           indent_str ^ "  Handlers: [" ^ hvars_str ^ "]\n" ^ *)
        ^ indent_str
        ^ "  Args: [" ^ es_str ^ "]"
    | Resume (e1, e2_opt) ->
        let e2_str =
          match e2_opt with
          | None -> "None"
          | Some e2 -> "Some " ^ expr_to_string ~indent:(indent + 1) e2
        in
        "Resume\n" ^ indent_str ^ "  "
        ^ expr_to_string ~indent:(indent + 1) e1
        ^ "\n" ^ indent_str ^ "  " ^ e2_str
    | Seq (e1, e2) ->
        "Seq\n" ^ indent_str ^ "  "
        ^ expr_to_string ~indent:(indent + 1) e1
        ^ "\n" ^ indent_str ^ "  "
        ^ expr_to_string ~indent:(indent + 1) e2
    | Aux af -> "Aux"
  in
  expr_str

and format_attrs attrs = ""
