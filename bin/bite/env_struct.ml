open Syntax
open Common
open Sexplib.Std

type fun_info = {
  fun_name : name;
  pfun_name : name option;
  locals : (name * ty) list;
  handlers : name list;
}
[@@deriving sexp]

let rec get_fun_info ((exp, attrs) : expr) (pfun_name : string option) :
    fun_info list * name list =
  let ( @++@ ) (l1, l2) (l1', l2') = (l1 @ l1', l2 @ l2') in
  match exp with
  | Var x -> ([], [])
  | Times (e1, e2) -> get_fun_info e1 pfun_name @++@ get_fun_info e2 pfun_name
  | Plus (e1, e2) -> get_fun_info e1 pfun_name @++@ get_fun_info e2 pfun_name
  | Minus (e1, e2) -> get_fun_info e1 pfun_name @++@ get_fun_info e2 pfun_name
  | Equal (e1, e2) -> get_fun_info e1 pfun_name @++@ get_fun_info e2 pfun_name
  | Less (e1, e2) -> get_fun_info e1 pfun_name @++@ get_fun_info e2 pfun_name
  | Assign (e1, e2) -> get_fun_info e1 pfun_name @++@ get_fun_info e2 pfun_name
  | Deref e -> get_fun_info e pfun_name
  | If (e1, e2, e3) ->
      get_fun_info e1 pfun_name @++@ get_fun_info e2 pfun_name
      @++@ get_fun_info e3 pfun_name
  | Let (x, e1, e2) -> get_fun_info e1 pfun_name @++@ get_fun_info e2 pfun_name
  | Decl (x, e1, e2) -> get_fun_info e1 pfun_name @++@ get_fun_info e2 pfun_name
  | Handle (x, fname, exp_catch, exp_handle) ->
      let[@warning "-partial-match"] FullFun (fun_name, _, _, _, _, _, _), _ =
        exp_catch
      in
      ([], [ fun_name ])
      @++@ get_fun_info exp_catch pfun_name
      @++@ get_fun_info exp_handle pfun_name
  | FullFun (fun_name, es1, hs, tm_args, ty, es2, exp_body) ->
      let hd_args =
        List.map (fun rhvar -> (rhvar.name, rhvar.ty)) attrs.hvarParams
      in
      let fun_infos, handlers = get_fun_info exp_body (Some fun_name) in
      ( {
          fun_name;
          pfun_name;
          locals = tm_args @ hd_args @ gather_locals exp_body;
          handlers;
        }
        :: fun_infos,
        [] )
  | FullApply (exp, es, hs, exps) ->
      get_fun_info exp pfun_name
      @++@ List.fold_left
             (fun acc exp_iter -> get_fun_info exp_iter pfun_name @++@ acc)
             ([], []) exps
  | Raise (h, es, hs, exps) ->
      List.fold_left
        (fun acc exp_iter -> get_fun_info exp_iter pfun_name @++@ acc)
        ([], []) exps
  | Resume e -> get_fun_info e pfun_name
  | Seq (e1, e2) -> get_fun_info e1 pfun_name @++@ get_fun_info e2 pfun_name
  | Int _ | Bool _ -> ([], [])

let local_to_struct_field (x, ty) : string =
  match ty with
  | TAbs _ ->
      spf "void *%s_fptr;\n" x ^ spf "void *%s_env;\n" x
      ^ spf "jmp_buf *%s_jb;\n" x
  | _ -> spf "%s %s;\n" (ty_to_string ty) x

let get_env_struct { fun_name; pfun_name; locals; handlers } : string =
  let locals = List.map local_to_struct_field locals |> String.concat "" in
  let jbs =
    List.map (fun h -> spf "jmp_buf %s_jb;\n" h) handlers |> String.concat ""
  in
  (match pfun_name with
  | None -> ""
  | Some pfun_name' -> spf "typedef %s_locals_t %s_env_t;\n" pfun_name' fun_name)
  ^ spf "typedef struct %s_locals_t {\n" fun_name
  ^ spf "%s_env_t* env;\n" fun_name
  ^ jbs ^ locals ^ "} "
  ^ spf "%s_locals_t;\n" fun_name
