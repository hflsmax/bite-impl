(* Abstract syntax. *)

(* Variable names *)
type name = string

(* Effect names *)
type fname = string

type hd =
  | Var of name

type eff =
  | Var of name
  | Handler of hd

(* Types *)
type ty =
  | TInt              (* Integers *)
  | TBool             (* Booleans *)
  | TUnit             (* Unit *)
  | TAbs of (eff list) * ((hd * fname) list) * (ty list) * ty * (eff list)
  | TArrow of ty * ty (* Functions *)


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
  | Handle of name * fname * expr * expr (* Handle [handle e1 : F = e1 in e2] *)
  | Fun of name * name * ty * ty * expr (* Function [fun f(x:s):t is e] *)
  | FullFun of name * eff list * (hd * fname) list * (name * ty) list * ty * eff list * expr
  | Apply of expr * expr 		(* Application [e1 e2] *)
  | Seq of expr * expr  		(* Sequence [e1; e2] *)

(* Toplevel commands *)
type command =
  | Expr of expr       (* Expression *)
  | Decl_eff of fname * ty
