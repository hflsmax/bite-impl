%{
  open Syntax
%}

%token TINT
%token TBOOL
%token TARROW
%token <Syntax.name> VAR
%token <int> INT
%token TRUE FALSE
%token PLUS
%token MINUS
%token TIMES
%token EQUAL LESS
%token IF THEN ELSE
%token FUN IS
%token FORALL DOT COMMA UNDERSCORE
%token COLON
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token EFF
%token LET
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
%right TARROW

%%

file:
  | EOF
    { [] }
  | e = expr EOF
    { [Expr e] }
  | e = expr SEMISEMI lst = file
    { Expr e :: lst }
  | ds = def EOF
    { [ds] }
  | ds = def SEMISEMI lst = file
    { ds :: lst }
  | eff = effect_declare EOF
    { [eff] }
  | eff = effect_declare SEMISEMI lst = file
    { eff :: lst }

toplevel:
  | d = def SEMISEMI
    { d }
  | e = expr SEMISEMI
    { Expr e }
  | eff = effect_declare SEMISEMI
    { eff }

def:
  | LET x = VAR EQUAL e = expr
    { Def (x, e) }

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
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr
    { If (e1, e2, e3) }
  | FUN x = VAR LPAREN f = VAR COLON t1 = ty RPAREN COLON t2 = ty IS e = expr
    { Fun (x, f, t1, t2, e) }

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
  | LPAREN e = plain_expr RPAREN	
    { e }    

// param:
//   | x = VAR COLON t = ty
//     { (Var x, t) }

// params:
//   | { [] }
//   | p = param
//     { [p] }
//   | p = param COMMA ps = params
//     { p :: ps }

hd_param:
  | x = VAR COLON t = VAR
    { (Var x, t) }

hd_params:
  | { [] }
  | p = hd_param
    { [p] }
  | p = hd_param COMMA ps = hd_params
    { Format.eprintf "hd_params"; p :: ps }

names:
  | { [] }
  | x = VAR
    { [Var x] }
  | x = VAR COMMA xs = names
    { Format.eprintf "names"; Var x :: xs }

ty:
  | TBOOL
    { TBool }
  | TINT
    { TInt }
  | t1 = ty TARROW t2 = ty
    { TArrow (t1, t2) }
  | LPAREN t = ty RPAREN
    { t }
  | FORALL LBRACKET es1 = names RBRACKET DOT 
    FORALL LBRACE hs = hd_params RBRACE DOT 
    LPAREN ts = tys RPAREN 
    TARROW t1 = ty UNDERSCORE LBRACKET es2 = names RBRACKET
    { Format.eprintf "TAbs"; TAbs (es1, hs, ts, t1, es2) }

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

