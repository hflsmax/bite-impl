open Syntax

let spf = Printf.sprintf

let full_fun_to_tabs (exp : expr') : ty =
  match exp with
  | FullFun (x, es1, hs, tm_args, ty, es2, exp) ->
      let tm_args_ty = List.split tm_args |> snd in
      TAbs (es1, hs, tm_args_ty, ty, es2)
  | _ -> failwith "full_fun_to_tabs: can only be called on FullFun"

(* Gather all local variables introduced within the scope of a function (excluding arguments)
   This is to create a list of local variables that represent the stacks.
*)
let rec gather_locals ((exp, attrs) : expr) : locals =
  match exp with
  | Times (e1, e2)
  | Plus (e1, e2)
  | Minus (e1, e2)
  | Equal (e1, e2)
  | Less (e1, e2) ->
      gather_locals e1 @ gather_locals e2
  | Assign (x, e) -> gather_locals e
  | If (e1, e2, e3) -> gather_locals e1 @ gather_locals e2 @ gather_locals e3
  | Let (x, e1, e2) -> ((x, (snd e1).ty) :: gather_locals e1) @ gather_locals e2
  | Decl (x, e1, e2) ->
      ((x, (snd e1).ty) :: gather_locals e1) @ gather_locals e2
  | Handle (x, _, catch_exp, handle_exp) ->
      ((x, (snd catch_exp).ty) :: gather_locals catch_exp)
      @ gather_locals handle_exp
  | FullFun (x, _, _, _, _, _, _) -> []
  | FullApply (e1, _, _, e2) ->
      gather_locals e1
      @ List.fold_left (fun acc exp_iter -> acc @ gather_locals exp_iter) [] e2
  | Raise (_, _, _, e) ->
      List.fold_left (fun acc exp_iter -> acc @ gather_locals exp_iter) [] e
  | Resume e -> gather_locals e
  | Seq (e1, e2) -> gather_locals e1 @ gather_locals e2
  | Var _ | Int _ | Bool _ | Deref _ -> []

(* Gather all free variables in an expression. It's computed by finding all used variables that are not bound *)
(* let rec gather_free_vars ((exp, ty, _, attrs) : expr) : locals =
   let exclude name locals =
     List.filter (fun (name', _) -> name <> name') locals
   in
   let exclude_all names locals =
     List.filter (fun (name', _) -> not (List.mem name' names)) locals
   in
   match exp with
   | Times (e1, e2)
   | Plus (e1, e2)
   | Minus (e1, e2)
   | Equal (e1, e2)
   | Less (e1, e2) ->
       gather_free_vars e1 @ gather_free_vars e2
   | Assign (x, e) ->
       let[@warning "-partial-match"] Var (_, name), ty, _, _ = x in
       (name, ty) :: gather_free_vars e
   | If (e1, e2, e3) ->
       gather_free_vars e1 @ gather_free_vars e2 @ gather_free_vars e3
   | Let (x, ty, e1, e2) -> gather_free_vars e1 @ exclude x (gather_free_vars e2)
   | Decl (x, ty, e1, e2) ->
       gather_free_vars e1 @ exclude x (gather_free_vars e2)
   | Handle (x, (_, ty), catch_exp, handle_exp) ->
       gather_free_vars catch_exp @ exclude x (gather_free_vars handle_exp)
   | FullFun (_, x, _, hparams, tparams, _, _, body) ->
       let hparams' = List.map (fun (name, _, _) -> name) hparams in
       let tparams' = List.map (fun (name, _) -> name) tparams in
       exclude_all ((x :: hparams') @ tparams') (gather_free_vars body)
   | FullApply (lhs, _, hargs, targs) ->
       let hvars = List.map (fun (name, _, ty) -> (name, ty)) hargs in
       gather_free_vars lhs @ hvars
       @ List.fold_left
           (fun acc exp_iter -> acc @ gather_free_vars exp_iter)
           [] targs
   | Raise ((name, _, ty), _, hargs, targs) ->
       let hvars = List.map (fun (name, _, ty) -> (name, ty)) hargs in
       ((name, ty) :: hvars)
       @ List.fold_left
           (fun acc exp_iter -> acc @ gather_free_vars exp_iter)
           [] targs
   | Resume e -> gather_free_vars e
   | Seq (e1, e2) -> gather_free_vars e1 @ gather_free_vars e2
   | Deref x -> gather_free_vars x
   | Var (_, x) -> [ (x, ty) ]
   | Int _ | Bool _ -> [] |> List.sort_uniq compare *)

let rec ty_to_string ty : string =
  match ty with
  | TBuiltin -> "void*"
  | TInt -> "int"
  | TBool -> "bool"
  | TMut ty -> ty_to_string ty
  | TAbs _ -> "closure_t"

let tabs_to_string ty is_handler : string =
  match ty with
  | TAbs (es1, hs, ty_args, ty, es2) ->
      (* The first parameter is the env pointer. *)
      let ty_args =
        ("void*" :: (if is_handler then [ "jmp_buf*" ] else []))
        @ List.map (fun ty_arg -> ty_to_string ty_arg) ty_args
      in
      let handler_ty_args =
        List.init (List.length hs) (fun _ -> "void*, void*, jmp_buf*")
      in
      Printf.sprintf "%s(*)(%s)" (ty_to_string ty)
        (String.concat ", " (ty_args @ handler_ty_args))
  | _ -> failwith "tabs_to_string: can only be called on TAbs"

let extra_defs arch =
  Sjlj.sjlj_def arch
  ^ {|
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "klist.h"

typedef struct closture_t {
    void *f_ptr;
    void *env;
    jmp_buf jb;
} closure_t;

volatile int jmpret;

typedef struct main_env_t {} main_env_t;

void* ArrayInit(int size) {
    return malloc(size * sizeof(int));
}

int ArrayGet(void* arr, int index) {
    return ((int*)arr)[index];
}

void noop(void* _) {}
KLIST_INIT(int_list, int, noop)

void* ListInit(int n) {
  klist_t(int_list)* list = (void*)kl_init(int_list);
  for (int i = 0; i < n; i++) {
    *kl_pushp(int_list, list) = i;
  }
  return list;
}

void ListPush(void* list, int i) {
  *kl_pushp(int_list, (klist_t(int_list)*)list) = i;
}

void ListShift(void* list) {
  kl_shift(int_list, (klist_t(int_list)*)list);
}

void* ListGetIter(void* list) {
  return kl_begin((klist_t(int_list)*)list);
}

void IterRemoveNext(void* iter) {
  kl_remove_next(int_list, iter);
}

bool IterHasNext(void* iter) {
  return ((kliter_t(int_list)*)iter)->next != NULL;
}

void* IterNext(void* iter) {
  return ((kliter_t(int_list)*)iter)->next;
}

void IterSet(void* iter, int val) {
  ((kliter_t(int_list)*)iter)->data = val;
}

int IterGet(void* iter) {
  return ((kliter_t(int_list)*)iter)->data;
}

int Print(int x) {
  printf("%d\n", x);
  return 0;
}
|}

let fst3 (x, _, _) = x
let snd3 (_, x, _) = x
let trd3 (_, _, x) = x

let cleanup s =
  let rec remove_semisemi (s : string) : string =
    let re = Str.regexp ";[ \n\r\x0c\t]*;" in
    try
      ignore (Str.search_forward re s 0);
      remove_semisemi (Str.global_replace re ";" s)
    with Not_found -> s
  in
  s
  |> Str.global_replace
       (Str.regexp_string "int main(closure_t* closure)")
       "int main()"
  |> remove_semisemi
