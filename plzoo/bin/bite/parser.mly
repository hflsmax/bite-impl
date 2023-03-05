%{
  open Syntax
%}

%token TINT
%token TBOOL
%token TUNIT
%token TARROW
%token <Syntax.name> VAR
%token <int> INT
%token UNIT
%token TRUE FALSE
%token PLUS
%token MINUS
%token TIMES
%token EQUAL LESS
%token IF THEN ELSE
%token FUN IS
%token FORALL DOT COMMA UNDERSCORE TILDE
%token COLON
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token EFF
%token DECL
%token HANDLE
%token LET IN END
%token ASSIGN
%token SEMI
%token SEMISEMI
%token EOF

%start file
%type <Syntax.command list> file

%start toplevel
%type <Syntax.command> toplevel

%nonassoc IS
%nonassoc ELSE
%nonassoc EQUAL LESS
%left PLUS MINUS
%left TIMES
%nonassoc ASSIGN
%right TARROW
%left SEMI

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

expr: mark_position(plain_expr) { $1 }
plain_expr:
  | e = plain_app_expr
    { e }
  | MINUS n = INT
    { Int (-n) }
  | e1 = expr PLUS e2 = expr	
    { Plus (e1, e2) }
  | e1 = expr MINUS e2 = expr
    { Minus (e1, e2) }
  | e1 = expr TIMES e2 = expr
    { Times (e1, e2) }
  | e1 = expr EQUAL e2 = expr
    { Equal (e1, e2) }
  | e1 = expr LESS e2 = expr
    { Less (e1, e2) }
  | x = VAR ASSIGN e = expr
    { Assign (x, e) }
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr 
    { If (e1, e2, e3) }
  | FUN x = VAR LBRACKET es1 = names RBRACKET LBRACE hs = hd_params RBRACE LPAREN args = params RPAREN COLON t = ty UNDERSCORE LBRACKET es2 = names RBRACKET IS e = expr 
    { FullFun (x, es1, hs, args, t, es2, e) }
  | LET x = VAR EQUAL e1 = expr IN e2 = expr END
    { Let (x, e1, e2) }
  | DECL x = VAR ASSIGN e1 = expr IN e2 = expr END
    { Decl (x, e1, e2) }
  | HANDLE x = VAR COLON fname = VAR EQUAL e1 = expr IN e2 = expr END
    { Handle (x, fname, e1, e2) }
  | e1 = expr SEMI e2 = expr
    { Seq (e1, e2) }

app_expr: mark_position(plain_app_expr) { $1 }
plain_app_expr:
  | e = plain_simple_expr
    { e }
  | e1 = app_expr e2 = simple_expr
    { Apply (e1, e2) }

simple_expr: mark_position(plain_simple_expr) { $1 }
plain_simple_expr:
  | x = VAR
    { Var x }
  | TRUE    
    { Bool true }
  | FALSE
    { Bool false }
  | n = INT
    { Int n }
  | UNIT
    { Unit }
  | LPAREN e = plain_expr RPAREN	
    { e }    

param:
  | x = VAR COLON t = ty
    { (x, t) }

params:
  | { [] }
  | p = param
    { [p] }
  | p = param COMMA ps = params
    { p :: ps }

hd_param:
  | x = VAR COLON t = VAR
    { (HVar x, t) }

hd_params:
  | { [] }
  | p = hd_param
    { [p] }
  | p = hd_param COMMA ps = hd_params
    { p :: ps }

names:
  | { [] }
  | x = VAR
    { [EVar x] }
  | TILDE x = VAR
    { [Handler (HVar x)] }
  | x = VAR COMMA xs = names
    { EVar x :: xs }

ty:
  | TBOOL
    { TBool }
  | TINT
    { TInt }
  | TUNIT
    { TUnit }
  | t1 = ty TARROW t2 = ty
    { TArrow (t1, t2) }
  | LPAREN t = ty RPAREN
    { t }
  | FORALL LBRACKET es1 = names RBRACKET DOT 
    FORALL LBRACE hs = hd_params RBRACE DOT 
    LPAREN ts = tys RPAREN 
    TARROW t1 = ty UNDERSCORE LBRACKET es2 = names RBRACKET
    { TAbs (es1, hs, ts, t1, es2) }

tys:
  | { [] }
  | t = ty
    { [t] }
  | t = ty COMMA ts = tys
    { t :: ts }

effect_declare:
  | EFF x = VAR COLON t = ty
    { Decl_eff (x, t) }

mark_position(X):
  x = X
  { Zoo.locate ~loc:(Zoo.make_location $startpos $endpos) x }

%%

