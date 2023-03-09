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
  | TAbs of effs * (hvar * (fname * ty)) list * tys * ty * effs
  | TMut of ty
  | TStackClosure of ty
  (* TODO: add THeapClosure *)
[@@deriving sexp]
and tys = ty list
[@@deriving sexp]

type f_ENV = (fname * ty) list
[@@deriving sexp]

type e_ENV = effs
[@@deriving sexp]

type h_ENV = (hvar * fname) list
[@@deriving sexp]

type t_ENV = (name * ty) list
[@@deriving sexp]


(* Expressions *)
type expr = expr' Zoo.located
and expr' =
  | Var of int * name (* int indicates the depth of var within the static link. It is not used in the source language *)
  | Int of int  		
  | Bool of bool		
  | Times of expr * expr 		
  | Plus of expr * expr  		
  | Minus of expr * expr 		
  | Equal of expr * expr 		
  | Less of expr * expr  		
  (* NOTE: the first component of Assign and Deref must be a Var. We choose to use the type "expr" instead of "name"
    because we want to annotate them with depth, like a Var *)
  | Assign of expr * expr 	
  | Deref of expr 			 
  | If of expr * expr * expr 		
  | Let of name * ty * expr * expr 		
  | Decl of name * ty * expr * expr 		  
  (* NOTE: ty of effect name is redundant, it's used to simplify future passes *)
  | Handle of name * (fname * ty) * expr * expr 
  | FullFun of name * effs * (hvar * (fname * ty)) list * (name * ty) list * ty * effs * expr
  (* NOTE: ty of lhs is added. It's used to cast a C function pointer to the appropriate C type *)
  | FullApply of (expr * ty) * effs * hvar list * expr list
  | Raise of hvar * effs * hvar list * expr list
  | Seq of expr * expr  		
[@@deriving sexp]

(* Toplevel commands *)
type command =
  | Expr of expr (* Expression *)
  | Decl_eff of fname * ty

type locals = (name * ty) list
[@@deriving sexp]
type static_link = locals list
[@@deriving sexp]
