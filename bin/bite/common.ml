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
  | AOP (_, e1, e2) | BOP (_, e1, e2) -> gather_locals e1 @ gather_locals e2
  | UOP (_, e) -> gather_locals e
  | Assign (x, e) -> gather_locals e
  | If (e1, e2, e3) -> gather_locals e1 @ gather_locals e2 @ gather_locals e3
  | Let (x, isTop, e1, e2) ->
      if isTop then gather_locals e1 @ gather_locals e2
      else ((x, (snd e1).ty) :: gather_locals e1) @ gather_locals e2
  | Decl (x, isTop, e1, e2) ->
      if isTop then gather_locals e1 @ gather_locals e2
      else ((x, (snd e1).ty) :: gather_locals e1) @ gather_locals e2
  | Handle (x, _, catch_exp, handle_exp) ->
      ((x, (snd catch_exp).ty) :: gather_locals catch_exp)
      @ gather_locals handle_exp
  | FullFun (x, _, _, _, _, _, _) -> []
  | FullApply (e1, _, _, e2) ->
      gather_locals e1
      @ List.fold_left (fun acc exp_iter -> acc @ gather_locals exp_iter) [] e2
  | Raise (_, _, _, e) ->
      List.fold_left (fun acc exp_iter -> acc @ gather_locals exp_iter) [] e
  | Resume (e, r) -> gather_locals e
  | Seq (e1, e2) -> gather_locals e1 @ gather_locals e2
  | Var _ | Int _ | Bool _ | Aux _ | Unit | Deref _ -> []

let rec ty_to_string ty : string =
  match ty with
  | TBuiltin -> "void*"
  | TInt -> "int"
  | TBool -> "bool"
  | TMut ty -> ty_to_string ty
  | TAbs _ -> "void*"
  | TCustom s -> s
  | TUnit -> "void"
  | TCont _ -> "void*"

let tabs_to_string ty is_handler : string =
  match ty with
  | TAbs (es1, hs, ty_args, ty, es2) ->
      let ty_args = List.map (fun ty_arg -> ty_to_string ty_arg) ty_args in
      spf "%s(*)(%s)" (ty_to_string ty) (String.concat ", " ty_args)
  | _ -> failwith "tabs_to_string: can only be called on TAbs"

let extra_defs arch =
  Sjlj.sjlj_def arch
  ^ {|
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include <mprompt.h>

#include "linkedList.h"

volatile int jmpret;

typedef struct main_env_t {} main_env_t;

#define ArrayInitStatic(size) (&((int[size]){0}))

void* ArrayInit(int size) {
    return malloc(size * sizeof(int));
}

int ArrayGet(void* arr, int index) {
    return ((int*)arr)[index];
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

let rec get_last (l : 'a list) : 'a =
  match l with
  | [] -> failwith "get_last: empty list"
  | [ x ] -> x
  | x :: xs -> get_last xs

let rec remove_last (l : 'a list) : 'a list =
  match l with [] -> [] | [ x ] -> [] | x :: xs -> x :: remove_last xs

let remove_cont_from_abs (ty : ty) : ty =
  let[@warning "-partial-match"] (TAbs (es1, hs, ty_args, ty0, es2)) = ty in
  if List.length ty_args > 0 then
    match List.hd (List.rev ty_args) with
    | TCont _ -> TAbs (es1, hs, remove_last ty_args, ty0, es2)
    | _ -> ty
  else ty

let special_case_env (x, ty) =
  if String.ends_with ~suffix:"_env" x then ("env", ty) else (x, ty)

let special_case_env2 x = if String.ends_with ~suffix:"_env" x then "env" else x
