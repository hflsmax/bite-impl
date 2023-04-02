(* Abstract syntax. *)

open Sexplib.Std
open Util

(* Variable names *)
type name = string [@@deriving sexp]

type handlerKind = TailResumptive | Abortive | Multishot | SingleShot
[@@deriving sexp]

(* Effect names *)
type fname = string [@@deriving sexp]
type hvar = string [@@deriving sexp]
type eff = EVar of string | HVar of hvar [@@deriving sexp]
type effs = eff list [@@deriving sexp]

(* Types *)
type ty =
  | TBuiltin (* Types like array that are not fully supported *)
  | TInt (* Integers *)
  | TBool (* Booleans *)
  | TAbs of effs * (hvar * fname) list * tys * ty * effs
  | TMut of ty
  | TCustom of string
  | TUnit
[@@deriving sexp]

and tys = ty list [@@deriving sexp]

type f_ENV = (fname * ty) list [@@deriving sexp]
type e_ENV = effs [@@deriving sexp]
type h_ENV = (hvar * fname) list [@@deriving sexp]
type t_ENV = (name * ty) list [@@deriving sexp]

let builtin_fun =
  [
    ("ArrayInitStatic", TAbs ([], [], [ TInt ], TBuiltin, []));
    ("ArrayInit", TAbs ([], [], [ TInt ], TBuiltin, []));
    ("ArrayGet", TAbs ([], [], [ TBuiltin; TInt ], TInt, []));
    ("ListNew", TAbs ([], [], [], TBuiltin, []));
    ("ListNewStatic", TAbs ([], [], [], TBuiltin, []));
    ("ListInit", TAbs ([], [], [ TBuiltin; TInt ], TBuiltin, []));
    ("ListAppend", TAbs ([], [], [ TBuiltin; TBuiltin ], TUnit, []));
    ("ListPopFirstElement", TAbs ([], [], [ TBuiltin ], TBuiltin, []));
    ("ListRemoveFirstElement", TAbs ([], [], [ TBuiltin ], TUnit, []));
    ("ListGetIter", TAbs ([], [], [ TBuiltin ], TBuiltin, []));
    ("IterHasNext", TAbs ([], [], [ TBuiltin ], TBool, []));
    ("IterNext", TAbs ([], [], [ TBuiltin ], TBuiltin, []));
    ("IterSet", TAbs ([], [], [ TBuiltin; TBuiltin ], TUnit, []));
    ("IterGet", TAbs ([], [], [ TBuiltin ], TBuiltin, []));
    ("IterSetInt", TAbs ([], [], [ TBuiltin; TInt ], TUnit, []));
    ("IterGetInt", TAbs ([], [], [ TBuiltin ], TInt, []));
    ("IterRemoveNext", TAbs ([], [], [ TBuiltin ], TUnit, []));
    ("Print", TAbs ([], [], [ TInt ], TInt, []));
    ("ReifyResumer", TAbs ([], [], [], TBuiltin, []));
  ]

(* Control-flow destination *)
type cf_dest = Return | Abort | Continue (* Neither return or abort. *)
[@@deriving sexp]

type richHvar = { name : name; fname : fname; ty : ty; depth : int }
[@@deriving sexp]

type attrs = {
  loc : location;
  isRecursiveCall : bool;
  topLevelFunctionName : string option;
  cfDest : cf_dest;
  isOptimizedSjlj : bool;
  isBuiltin : bool;
  varDepth : int option;
  ty : ty;
  effs : effs;
  hvarParams : richHvar list option; (* Used in FullFun *)
  hvarArgs : richHvar list option; (* Used in FullApply and Raise *)
  lhsHvar : richHvar option; (* Used in Raise *)
  bindHvar : richHvar option; (* Used in Handle *)
  handlerKind : handlerKind option; (* Used in Handle *)
  isHandler : bool; (* Used in FullFun *)
  freeVars : (name * ty) list; (* Used in FullFun *)
}
[@@deriving sexp]

let default_attrs =
  {
    loc = Nowhere;
    isRecursiveCall = false;
    topLevelFunctionName = None;
    isBuiltin = false;
    isOptimizedSjlj = false;
    cfDest = Continue;
    varDepth = None;
    ty = TInt;
    effs = [];
    hvarParams = None;
    hvarArgs = None;
    lhsHvar = None;
    bindHvar = None;
    handlerKind = None;
    isHandler = false;
    freeVars = [];
  }

let locate ?(loc = Nowhere) x = (x, { default_attrs with loc })

(* Expressions *)
type expr = expr' * attrs

and expr' =
  | Var of name
  | Int of int
  | Bool of bool
  | Unit
  | AOP of string * expr * expr
  | BOP of string * expr * expr
  (* NOTE: the first component of Assign and Deref must be a Var. We choose to use the type "expr" instead of "name"
     because we want to annotate them with depth, like a Var *)
  | Assign of expr * expr
  | Deref of expr
  | If of expr * expr * expr
  | Let of name * bool * expr * expr
  | Decl of name * bool * expr * expr
  | Handle of name * fname * expr * expr
  | FullFun of
      name * effs * (hvar * fname) list * (name * ty) list * ty * effs * expr
  | FullApply of expr * effs * hvar list * expr list
  | Raise of hvar * effs * hvar list * expr list
  | Resume of expr * expr option
  | Seq of expr * expr
[@@deriving sexp]

(* Toplevel commands *)
type command =
  | Expr of expr
  (* Expression *)
  | Decl_eff of fname * ty

type locals = (name * ty) list [@@deriving sexp]
type static_link = locals list [@@deriving sexp]
