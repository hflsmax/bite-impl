(* Abstract syntax. *)

open Util

(* Variable names *)
type name = string [@@deriving yojson_of]

type handlerKind = TailResumptive | Abortive | Multishot | SingleShot
[@@deriving yojson_of]

(* Effect names *)
type fname = string [@@deriving yojson_of]
type hvar = string [@@deriving yojson_of]
type eff = EVar of string | HVar of hvar [@@deriving yojson_of]
type effs = eff list [@@deriving yojson_of]

(* Types *)
type ty =
  | TBuiltin (* Types like array that are not fully supported *)
  | TInt (* Integers *)
  | TBool (* Booleans *)
  | TAbs of effs * (hvar * fname) list * tys * ty * effs
  | TMut of ty
  | TCustom of string
  | TUnit
[@@deriving yojson_of]

and tys = ty list [@@deriving yojson_of]

type f_ENV = (fname * ty) list [@@deriving yojson_of]
type e_ENV = effs [@@deriving yojson_of]
type h_ENV = (hvar * fname) list [@@deriving yojson_of]
type t_ENV = (name * ty) list [@@deriving yojson_of]

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
    ("!setjmp", TAbs ([], [], [ TBuiltin ], TInt, []));
    ("longjmp", TAbs ([], [], [ TBuiltin; TInt ], TInt, []));
  ]

(* Control-flow destination *)
type cf_dest = Return | Continue [@@deriving yojson_of]

type richHvar = { name : name; fname : fname; ty : ty; depth : int }
[@@deriving yojson_of]

type attrs = {
  loc : location; [@yojson_drop_if fun _ -> true]
  isRecursiveCall : bool; [@yojson_drop_if fun _ -> true]
      (* Used in FullApply *)
  isTopCall : bool; [@yojson_drop_if fun _ -> true] (* Used in FullApply *)
  cfDest : cf_dest; [@yojson_drop_if fun _ -> true]
  isOptimizedSjlj : bool; [@yojson_drop_if fun _ -> true]
  isBuiltin : bool; [@yojson_drop_if fun _ -> true]
  isDeclareOnly : bool; [@yojson_drop_if fun _ -> true] (* Used in Let *)
  varDepth : int option; [@yojson_drop_if fun _ -> true]
  ty : ty; [@yojson_drop_if fun _ -> true]
  effs : effs; [@yojson_drop_if fun _ -> true]
  hvarParams : richHvar list option; [@yojson_drop_if fun _ -> true]
      (* Used in FullFun *)
  hvarArgs : richHvar list option; [@yojson_drop_if fun _ -> true]
      (* Used in FullApply and Raise *)
  lhsHvar : richHvar option; [@yojson_drop_if fun _ -> true] (* Used in Raise *)
  bindHvar : richHvar option; [@yojson_drop_if fun _ -> true]
      (* Used in Handle *)
  handlerKind : handlerKind option; [@yojson_drop_if fun _ -> true]
      (* Used in Handle *)
  isHandler : bool; [@yojson_drop_if fun _ -> true] (* Used in FullFun *)
  freeVars : (name * ty) list; [@yojson_drop_if fun _ -> true]
      (* Used in FullFun *)
  freeVarsOfBody : (name * ty) list; [@yojson_drop_if fun _ -> true]
      (* Used in FullFun *)
}
[@@deriving yojson_of]

let default_attrs =
  {
    loc = Nowhere;
    isRecursiveCall = false;
    isTopCall = false;
    isBuiltin = false;
    isOptimizedSjlj = false;
    isDeclareOnly = false;
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
    freeVarsOfBody = [];
  }

let locate ?(loc = Nowhere) x = (x, { default_attrs with loc })

type auxFunction =
  | ReifyResumer
  | ReifyEnvironment (* Environment of closure *)
  | ReifyFixedContext (* Evaluation context for abortive handler *)
  | ReifyContextIndirection (* Evaluation context for general handler *)
  | ReifyContinuation
  | Noop
[@@deriving yojson_of]

(* Expressions *)
type expr = expr' * attrs [@@deriving yojson_of]

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
  | Aux of auxFunction
[@@deriving yojson_of]

(* Toplevel commands *)
type command =
  | Expr of expr
  (* Expression *)
  | Decl_eff of fname * ty

type locals = (name * ty) list [@@deriving yojson_of]
type static_link = (name * bool) list list [@@deriving yojson_of]

let mk_builtin_fun (name : string) : expr =
  let ty = List.assoc name builtin_fun in
  (Var name, { default_attrs with ty; isBuiltin = true })

let mk_var ?(ty = TInt) (name : name) : expr =
  (Var name, { default_attrs with ty })

let mk_int (i : int) : expr = (Int i, { default_attrs with ty = TInt })
