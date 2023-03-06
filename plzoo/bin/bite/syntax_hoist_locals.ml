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
  | TInt  (* Integers *)
  | TBool (* Booleans *)
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
type expr =
  | Var of name 		
  | Int of int  		
  | Bool of bool		
  | Times of expr * expr 		
  | Plus of expr * expr  		
  | Minus of expr * expr 		
  | Equal of expr * expr 		
  | Less of expr * expr  		
  | Assign of name * expr 	
  | Deref of name 			 
  | If of expr * expr * expr 		
  | Let of name * expr * expr 		
  | Decl of name * expr * expr 		  
  | Handle of name * fname * expr * expr 
  | FullFun of name * effs * hds * (name * ty) list * ty * effs * expr
  | FullApply of expr * effs * hvar list * expr list
  | Raise of hvar * effs * hvar list * expr list
  | Seq of expr * expr  		
[@@deriving sexp]

(* Toplevel commands *)
type command =
  | Expr of expr (* Expression *)
  | Decl_eff of fname * ty
