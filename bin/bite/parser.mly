%{
  open Syntax
  open Util
%}

%token TUNIT
%token TINT
%token TBOOL
%token ARROW
%token TBUILTIN
%token <Syntax.name> VAR
%token <int> INT
%token UNIT
%token NOTHING
%token TRUE FALSE
%token PLUS MINUS TIMES DIV MOD
%token EQUAL LESS GREATER LEQ GEQ NEQ AND OR NOT
%token IF THEN ELSE
%token FUN FN IS
%token RAISE RESUME
%token FORALL DOT COMMA UNDERSCORE TILDE
%token COLON
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token EFF
%token DECL
%token HANDLE
%token LET IN END
%token ASSIGN
%token BANG
%token SEMI
%token SEMISEMI
%token EOF
%token POW

%start file
%type <Syntax.command list> file

%start toplevel
%type <Syntax.command> toplevel

// %nonassoc IS
%nonassoc ELSE
%left SEMI
%nonassoc RESUME
%nonassoc ASSIGN
%nonassoc EQUAL LESS GREATER LEQ GEQ NEQ AND OR NOT
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc BANG

%%

file:
  | EOF
    { [] }
  | e = expr EOF
    { [Expr e] }
  | e = expr SEMISEMI lst = file
    { Expr e :: lst }
  | eff = effect_declare EOF
    { [eff] }
  | eff = effect_declare SEMISEMI lst = file
    { eff :: lst }

toplevel:
  | e = expr SEMISEMI
    { Expr e }
  | eff = effect_declare SEMISEMI
    { eff }


tm_param:
  | COMMA? x = VAR COLON t = ty
    { (x, t) }

tm_params:
  | NOTHING
    { [] }
  | LPAREN args = tm_param+ RPAREN
    { args }

hd_param:
  | COMMA? x = VAR COLON t = VAR
    { (x, t) }

hd_arg:
  | x = VAR
    { x }

eff_name:
  | COMMA? x = VAR
    { EVar x }
  | COMMA? TILDE x = VAR
    { HVar (x) }

var: mark_position(plain_var) { $1 }
plain_var:
  | x = VAR
    { Var x }

lambda: mark_position(plain_lambda) { $1 }
plain_lambda:
  | FUN x = VAR LBRACKET es1 = eff_name* RBRACKET 
                LBRACE hs = hd_param* RBRACE 
                tm_params = tm_params
                COLON t = ty UNDERSCORE LBRACKET es2 = eff_name* RBRACKET IS e = expr END
    { FullFun (x, es1, hs, tm_params, t, es2, e) }
  | FN LBRACKET es1 = eff_name* RBRACKET 
                LBRACE hs = hd_param* RBRACE 
                tm_params = tm_params
                COLON t = ty UNDERSCORE LBRACKET es2 = eff_name* RBRACKET IS e = expr END
    { FullFun ("", es1, hs, tm_params, t, es2, e) }

lhs: mark_position(plain_lhs) { $1 }
plain_lhs:
  | x = plain_var
    { x }
  | f = plain_lambda
    { f }

effect_args:
  | LBRACKET es = eff_name* RBRACKET
    { es }

handler_args:
  | LBRACE hs = hd_arg* RBRACE
    { hs }

term_arg: mark_position(plain_term_arg) { $1 }
plain_term_arg:
  | x = plain_var
    { x }
  | TRUE    
    { Bool true }
  | FALSE
    { Bool false }
  | n = INT
    { Int n }
  | BANG x = var 
    { Deref x }
  | LPAREN e = plain_expr RPAREN	
    { e }

term_args:
  | NOTHING
    { [] }
  | args = term_arg+
    { args }

expr: mark_position(plain_expr) { $1 }
plain_expr:
  | LPAREN e = plain_expr RPAREN	
    { e }
  | lhs = lhs es = effect_args? hs = handler_args? exps = term_args
    { FullApply (lhs, (Option.value es ~default:[]), (Option.value hs ~default:[]), exps) }
  | RAISE hvar = VAR es = effect_args? hs = handler_args? exps = term_args
    { Raise (hvar, (Option.value es ~default:[]), (Option.value hs ~default:[]), exps) }
  | RESUME expr = expr
    { Resume (expr, None) }
  | x = plain_lhs
    { x }
  | TRUE    
    { Bool true }
  | FALSE
    { Bool false }
  | n = INT
    { Int n }
  | UNIT
    { Unit }
  | MINUS n = INT
    { Int (-n) }
  | BANG x = expr
    { Deref x }
  | e1 = expr PLUS e2 = expr	
    { AOP ("+", e1, e2) }
  | e1 = expr MINUS e2 = expr
    { AOP ("-", e1, e2) }
  | e1 = expr TIMES e2 = expr
    { AOP ("*", e1, e2) }
  | e1 = expr DIV e2 = expr
    { AOP ("/", e1, e2) }
  | e1 = expr MOD e2 = expr
    { AOP ("%", e1, e2) }
  | e1 = expr EQUAL e2 = expr
    { BOP ("==", e1, e2) }
  | e1 = expr LESS e2 = expr
    { BOP ("<", e1, e2) }
  | e1 = expr GREATER e2 = expr
    { BOP (">", e1, e2) }
  | e1 = expr LEQ e2 = expr
    { BOP ("<=", e1, e2) }
  | e1 = expr GEQ e2 = expr
    { BOP (">=", e1, e2) }
  | e1 = expr NEQ e2 = expr
    { BOP ("!=", e1, e2) }
  | e1 = expr AND e2 = expr
    { BOP ("&&", e1, e2) }
  | e1 = expr OR e2 = expr
    { BOP ("||", e1, e2) }
  | e = NOT e1 = expr
    { UOP ("!", e1) }
  | x = var ASSIGN e = expr
    { Assign (x, e) }
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr 
    { If (e1, e2, e3) }
  | LET p = POW? x = VAR EQUAL e1 = expr IN e2 = expr END
    { match p with
      | None -> Let (x, false, e1, e2)
      | Some _ -> Let (x, true, e1, e2) }
  | DECL p = POW? x = VAR ASSIGN e1 = expr IN e2 = expr END
    { match p with
      | None -> Decl (x, false, e1, e2)
      | Some _ -> Decl (x, true, e1, e2) }
  | HANDLE x = VAR COLON fname = VAR EQUAL e1 = lambda IN e2 = expr END
    { let [@warning "-partial-match"] (FullFun _, _) = e1 in
      Handle (x, fname, e1, e2) }
  | e1 = expr SEMI e2 = expr
    { Seq (e1, e2) }

tys:
  | NOTHING
    { [] }
  | LPAREN ts = ty+ RPAREN
    { ts }

ty:
  | COMMA? TBOOL
    { TBool }
  | COMMA? TINT
    { TInt }
  | COMMA? TUNIT
    { TUnit }
  | COMMA? TBUILTIN
    { TBuiltin }
  | COMMA? LPAREN t = ty RPAREN
    { t }
  | COMMA? FORALL LBRACKET es1 = eff_name* RBRACKET DOT 
    FORALL LBRACE hs = hd_param* RBRACE DOT 
    ts = tys
    ARROW t1 = ty UNDERSCORE LBRACKET es2 = eff_name* RBRACKET
    { TAbs (es1, hs, ts, t1, es2) }

effect_declare:
  | EFF x = VAR COLON t = ty
    { Decl_eff (x, t) }

mark_position(X):
  x = X
  { locate ~loc:(make_location $startpos $endpos) x }

%%

