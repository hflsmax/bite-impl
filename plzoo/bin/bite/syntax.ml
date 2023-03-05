(* Abstract syntax. *)

open Sexplib.Std

(* Variable names *)
type name = string
[@@deriving sexp]

(* Effect names *)
type fname = string
[@@deriving sexp]

type hd =
  | HVar of string
[@@deriving sexp]

type hds = (hd * fname) list
[@@deriving sexp]

type eff =
  | EVar of string
  | Handler of hd
[@@deriving sexp]

type effs = eff list
[@@deriving sexp]

(* Types *)
type ty =
  | TInt              (* Integers *)
  | TBool             (* Booleans *)
  | TUnit             (* Unit *)
  | TAbs of effs * hds * tys * ty * effs
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
  | Unit                 		(* Unit constant *)
  | Times of expr * expr 		(* Product [e1 * e2] *)
  | Plus of expr * expr  		(* Sum [e1 + e2] *)
  | Minus of expr * expr 		(* Difference [e1 - e2] *)
  | Equal of expr * expr 		(* Integer comparison [e1 = e2] *)
  | Less of expr * expr  		(* Integer comparison [e1 < e2] *)
  | Assign of name * expr 		(* Assignment [e1 := e2] *)
  | If of expr * expr * expr 		(* Conditional [if e1 then e2 else e3] *)
  | Let of name * expr * expr 		(* Local [let x = e1 in e2] *)
  | Decl of name * expr * expr 		  (* Local Assignable [dcl x := e1 in e2] *)
  | Handle of name * string * expr * expr (* Handle [handle e1 : F = e1 in e2] *)
  (* | Fun of name * name * ty * ty * expr *)
  | FullFun of name * effs * hds * (name * ty) list * ty * effs * expr
  | Apply of expr * expr 	
  | Seq of expr * expr  		(* Sequence [e1; e2] *)

(* Toplevel commands *)
type command =
  | Expr of expr       (* Expression *)
  | Decl_eff of string * ty
