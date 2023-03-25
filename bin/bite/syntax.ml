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
  | HVar of hvar
[@@deriving sexp]

type effs = eff list
[@@deriving sexp]

(* Types *)
type ty =
  | TBuiltin (* Types like array that are not fully supported *)
  | TInt  (* Integers *)
  | TBool (* Booleans *)
  | TAbs of effs * (hvar * fname) list * tys * ty * effs
  | TMut of ty
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

type lambda_kind = 
  | TailResumptive
  | Abortive
  | GeneralHandler
  | Lambda
[@@deriving sexp]

type builtin_fun = 
  | ArrayInit
  | ArrayGet

  | ListInit
  | ListPush
  | ListShift
  | ListGetIter

  | IterRemoveNext
  | IterHasNext
  | IterNext
  | IterSet
  | IterGet
[@@deriving sexp]
let builtin_fun = [
  "ArrayInit", ArrayInit; "ArrayGet", ArrayGet; 
  "ListInit", ListInit; "ListPush", ListPush; "ListShift", ListShift; "ListGetIter", ListGetIter;
  "IterRemoveNext", IterRemoveNext; "IterHasNext", IterHasNext; "IterNext", IterNext; "IterSet", IterSet; "IterGet", IterGet;
]

(* Expressions *)
type expr = expr' Zoo.located
and expr' =
  | Var of name
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
  | Let of name * expr * expr 		
  | Decl of name * expr * expr 		  
  | Handle of name * fname * expr * expr 
  | FullFun of lambda_kind * name option * effs * (hvar * fname) list * (name * ty) list * ty * effs * expr
  | FullApply of expr * effs * hvar list * expr list
  | Raise of hvar * effs * hvar list * expr list
  | Resume of expr
  | Seq of expr * expr
[@@deriving sexp]


(* Rich expression *)
module R = struct

(* Rich hvar *)
type hvar = string * fname * ty
[@@deriving sexp]

(* Control-flow destination *)
type cf_dest =
  | Return
  | Abort
  | Continue (* Neither return or abort. *)
[@@deriving sexp]

type attrs = { 
  isRecursiveCall: bool;
  topLevelFunctionName: string option;
  cfDest: cf_dest;
  isBuiltin: bool;
}
[@@deriving sexp]

let default_attrs = { isRecursiveCall = false; topLevelFunctionName = None; isBuiltin = false; cfDest = Continue; }

type expr = expr' * ty * effs * attrs
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
  | FullFun of lambda_kind * name * effs * hvar list * (name * ty) list * ty * effs * expr
  | FullApply of expr * effs * hvar list * expr list
  | Raise of hvar * effs * hvar list * expr list
  | Resume of expr
  | Seq of expr * expr  
[@@deriving sexp]
end

(* Toplevel commands *)
type command =
  | Expr of expr (* Expression *)
  | Decl_eff of fname * ty

type locals = (name * ty) list
[@@deriving sexp]
type static_link = locals list
[@@deriving sexp]
