(* Abstract syntax. *)

open Sexplib.Std

(* Variable names *)
type name = string
[@@deriving sexp]

(* Effect names *)
type fname = string
[@@deriving sexp]

type hvar = string
[@@deriving sexp]

type hds = (hvar * fname) list
[@@deriving sexp]

type eff =
  | EVar of string
  | Handler of hvar
[@@deriving sexp]

type effs = eff list
[@@deriving sexp]

(* Types *)
type ty =
  | TInt              (* Integers *)
  | TBool             (* Booleans *)
  | TAbs of effs * hds * tys * ty * effs
  | TMut of ty
  (* | TArrow of ty * ty  *)
[@@deriving sexp]
and tys = ty list
[@@deriving sexp]

type f_ENV = (fname * ty) list
[@@deriving sexp]

type e_ENV = effs
[@@deriving sexp]

type h_ENV = hds
[@@deriving sexp]

type t_ENV = (name * ty) list
[@@deriving sexp]


(* Expressions *)
type expr = expr' Zoo.located
and expr' =
  | Var of name          		(* Variable *)
  | Int of int           		(* Non-negative integer constant *)
  | Bool of bool         		(* Boolean constant *)
  | Times of expr * expr 		(* Product [e1 * e2] *)
  | Plus of expr * expr  		(* Sum [e1 + e2] *)
  | Minus of expr * expr 		(* Difference [e1 - e2] *)
  | Equal of expr * expr 		(* Integer comparison [e1 = e2] *)
  | Less of expr * expr  		(* Integer comparison [e1 < e2] *)
  | Assign of name * expr 	(* Assignment [e1 := e2] *)
  | Deref of name 			    (* Dereference [!e] *)
  | If of expr * expr * expr 		(* Conditional [if e1 then e2 else e3] *)
  | Let of name * expr * expr 		(* Local [let x = e1 in e2] *)
  | Decl of name * expr * expr 		  (* Local Assignable [dcl x := e1 in e2] *)
  | Handle of name * fname * expr * expr (* Handle [handle e1 : F = e1 in e2] *)
  (* | Fun of name * name * ty * ty * expr *)
  | FullFun of name * effs * hds * (name * ty) list * ty * effs * expr
  | FullApply of expr * effs * hvar list * expr list
  | Raise of hvar * effs * hvar list * expr list
  (* | EApply of expr * eff
  | HApply of expr * hd *)
  | Seq of expr * expr  		(* Sequence [e1; e2] *)
[@@deriving sexp]

(* Toplevel commands *)
type command =
  | Expr of expr       (* Expression *)
  | Decl_eff of fname * ty
