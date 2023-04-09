(* Abstract syntax. *)

open Util

(* Variable names *)
type name = string [@@deriving yojson_of]

(* NB: multishot is not supported *)
type handlerKind = TailResumptive | Abortive | MultiShot | SingleShot
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
  | TCont of ty * effs * ty * effs
[@@deriving yojson_of]

and tys = ty list [@@deriving yojson_of]

type f_ENV = (fname * (handlerKind * ty)) list [@@deriving yojson_of]
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
    ("ListIsEmpty", TAbs ([], [], [ TBuiltin ], TBool, []));
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
    ( "ListAppendCont",
      TAbs ([], [], [ TBuiltin; TCont (TUnit, [], TUnit, []) ], TUnit, []) );
    ( "ListPopFirstElementCont",
      TAbs ([], [], [ TBuiltin ], TCont (TUnit, [], TUnit, []), []) );
    ("setjmp", TAbs ([], [], [ TBuiltin ], TInt, []));
    ("longjmp", TAbs ([], [], [ TBuiltin; TInt ], TInt, []));
    ("mp_prompt", TAbs ([], [], [ TBuiltin; TBuiltin ], TUnit, []));
    ( "mp_yield",
      TAbs
        ( [],
          [],
          [
            TBuiltin;
            TAbs ([], [], [ TBuiltin; TBuiltin ], TBuiltin, []);
            TBuiltin;
          ],
          TUnit,
          [] ) );
  ]

(* Control-flow destination *)
type cf_dest = Return | Continue [@@deriving yojson_of]

type richHvar = {
  name : name;
  fname : fname;
  kind : handlerKind;
  ty : ty;
  depth : int;
}
[@@deriving yojson_of]

type attrs = {
  loc : location; [@yojson_drop_if fun _ -> true]
  recursiveCallFunName : string option; [@yojson_drop_if fun _ -> true]
      (* Used in FullApply *)
  isTopCall : bool; [@yojson_drop_if fun _ -> true] (* Used in FullApply *)
  cfDest : cf_dest; [@yojson_drop_if fun _ -> true]
  isRecursive : bool; [@yojson_drop_if fun _ -> true] (* Used in FullFun *)
  isOptimizedSjlj : bool; [@yojson_drop_if fun _ -> true]
  isBuiltin : bool; [@yojson_drop_if fun _ -> true]
  skipDef : bool; [@yojson_drop_if fun _ -> true] (* Used in Let *)
  defAtTop : bool; [@yojson_drop_if fun _ -> true] (* Used in Let *)
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
      (* Used in Handle and FullFun *)
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
    recursiveCallFunName = None;
    isTopCall = false;
    isBuiltin = false;
    isOptimizedSjlj = false;
    skipDef = false;
    defAtTop = false;
    cfDest = Continue;
    isRecursive = false;
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
  | UOP of string * expr
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
  | Decl_eff of fname * handlerKind * ty

type locals = (name * ty) list [@@deriving yojson_of]
type static_link = (name * bool) list list [@@deriving yojson_of]

let mk_builtin_fun (name : string) : expr =
  let ty = List.assoc name builtin_fun in
  (Var name, { default_attrs with ty; isBuiltin = true })

let mk_var ?(ty = TInt) (name : name) : expr =
  (Var name, { default_attrs with ty })

let mk_int (i : int) : expr = (Int i, { default_attrs with ty = TInt })
let mk_bool (b : bool) : expr = (Bool b, { default_attrs with ty = TBool })

let mk_uop (op : string) (e : expr) : expr =
  (UOP (op, e), { default_attrs with ty = TBool })

let mk_bop (op : string) (e1 : expr) (e2 : expr) : expr =
  (BOP (op, e1, e2), { default_attrs with ty = TBool })

let mk_apply_0 ?(ty = TInt) (e : expr) : expr =
  (FullApply (e, [], [], []), { default_attrs with ty })

let mk_apply_1 ?(ty = TInt) (e1 : expr) (e2 : expr) : expr =
  (FullApply (e1, [], [], [ e2 ]), { default_attrs with ty })

let mk_apply_2 ?(ty = TInt) (e1 : expr) (e2 : expr) (e3 : expr) : expr =
  (FullApply (e1, [], [], [ e2; e3 ]), { default_attrs with ty })

let mk_seq ?(ty = TInt) (e1 : expr) (e2 : expr) : expr =
  (Seq (e1, e2), { default_attrs with ty })

let mk_asgn (e1 : expr) (e2 : expr) : expr =
  (Assign (e1, e2), { default_attrs with ty = TUnit })

let mk_fun_0 ?(ty = TInt) (name : name) (body : expr) : expr =
  ( FullFun (name, [], [], [], ty, [], body),
    { default_attrs with ty = TAbs ([], [], [], ty, []) } )

let mk_fun_2 ?(ty = TInt) (f_name : name) (arg1_name : name) (arg1_ty : ty)
    (arg2_name : name) (arg2_ty : ty) (body : expr) : expr =
  ( FullFun
      ( f_name,
        [],
        [],
        [ (arg1_name, arg1_ty); (arg2_name, arg2_ty) ],
        ty,
        [],
        body ),
    { default_attrs with ty = TAbs ([], [], [ arg1_ty; arg2_ty ], ty, []) } )

let mk_let ?(ty = TInt) (name : name) (e1 : expr) (e2 : expr) : expr =
  (Let (name, false, e1, e2), { default_attrs with ty })

let mk_aux (f : auxFunction) : expr = (Aux f, { default_attrs with ty = TInt })
