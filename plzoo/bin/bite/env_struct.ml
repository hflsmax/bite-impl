
open Syntax
open Common
open Sexplib.Std


(* the name of the function, the name of the enclosing function, args and locals *)
type env_struct = name * name * (name * ty) list
[@@deriving sexp]

type fun_info = name * name option * (name * ty) list
[@@deriving sexp]

let rec get_fun_info ((exp, ty, effs): R.expr) (p_fun : string option) : fun_info list  =
  match exp with
  | Var (_, x) -> []
  | Times (e1, e2) -> get_fun_info e1 p_fun @ get_fun_info e2 p_fun
  | Plus (e1, e2) -> get_fun_info e1 p_fun @ get_fun_info e2 p_fun
  | Minus (e1, e2) -> get_fun_info e1 p_fun @ get_fun_info e2 p_fun
  | Equal (e1, e2) -> get_fun_info e1 p_fun @ get_fun_info e2 p_fun
  | Less (e1, e2) -> get_fun_info e1 p_fun @ get_fun_info e2 p_fun
  | Assign (e1, e2) -> get_fun_info e1 p_fun @ get_fun_info e2 p_fun
  | Deref e -> get_fun_info e p_fun
  | If (e1, e2, e3) ->
      get_fun_info e1 p_fun @ get_fun_info e2 p_fun @ get_fun_info e3 p_fun
  | Let (x, ty, e1, e2) ->
      get_fun_info e1 p_fun @ get_fun_info e2 p_fun
  | Decl (x, ty, e1, e2) ->
      get_fun_info e1 p_fun @ get_fun_info e2 p_fun
  | Handle (x, fname, exp_catch, exp_handle) ->
      get_fun_info exp_catch p_fun @ get_fun_info exp_handle p_fun
  | FullFun (x, es1, hs, tm_args, ty, es2, exp_body) ->
    let hd_args = List.map (fun (name, _, ty) -> (name, ty)) hs in
    (x, p_fun, (x, full_fun_to_tabs exp) :: tm_args @ hd_args @ gather_locals exp_body) :: get_fun_info exp_body (Some x)
  | FullApply (exp, es, hs, exps) ->
      get_fun_info exp p_fun @ List.fold_left (fun acc exp_iter -> get_fun_info exp_iter p_fun @ acc) [] exps
  | Raise (h, es, hs, exps) ->
      List.fold_left (fun acc exp_iter -> get_fun_info exp_iter p_fun @ acc) [] exps
  | Seq (e1, e2) ->
      get_fun_info e1 p_fun @ get_fun_info e2 p_fun
  | Int _  | Bool _  -> []

let get_env_struct (name, p_fun, locals) : string =
  let env_struct_decl = 
    let locals = List.map (fun (x, ty) -> spf "%s %s;\n" (ty_to_string ty) x) locals |> String.concat "" in
    (match p_fun with None -> "" | Some p_fun' -> spf "typedef %s_locals_t %s_env_t;\n" p_fun' name) ^
    spf "typedef struct %s_locals_t {\n" name ^
    spf "%s_env_t* env;\n" name ^
    locals ^ "} " ^
    spf "%s_locals_t;\n" name
  in
    env_struct_decl