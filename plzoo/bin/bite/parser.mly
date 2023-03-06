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

// %nonassoc IS
%nonassoc ELSE
%nonassoc EQUAL LESS
%left PLUS MINUS
%left TIMES
%nonassoc ASSIGN
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

lhs: mark_position(plain_lhs) { $1 }
plain_lhs:
  | x = VAR
    { Var x }
  | FUN x = VAR LBRACKET es1 = eff_name* RBRACKET 
                LBRACE hs = hd_param* RBRACE 
                LPAREN args = tm_param* RPAREN 
                COLON t = ty UNDERSCORE LBRACKET es2 = eff_name* RBRACKET IS e = expr END
    { FullFun (x, es1, hs, args, t, es2, e) }

effect_args:
  | LBRACKET es = eff_name* RBRACKET
    { es }

handler_args:
  | LBRACE hs = hd_arg* RBRACE
    { hs }

term_arg: mark_position(plain_term_arg) { $1 }
plain_term_arg:
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

expr: mark_position(plain_expr) { $1 }
plain_expr:
  | LPAREN e = plain_expr RPAREN	
    { e }
  | lhs = lhs es = effect_args? hs = handler_args? exps = term_arg+
    { FullApply (lhs, (Option.value es ~default:[]), (Option.value hs ~default:[]), exps) }
  | x = plain_lhs
    { x }
  | TRUE    
    { Bool true }
  | FALSE
    { Bool false }
  | n = INT
    { Int n }
  | MINUS n = INT
    { Int (-n) }
  | UNIT
    { Unit }
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
  | LET x = VAR EQUAL e1 = expr IN e2 = expr END
    { Let (x, e1, e2) }
  | DECL x = VAR ASSIGN e1 = expr IN e2 = expr END
    { Decl (x, e1, e2) }
  | HANDLE x = VAR COLON fname = VAR EQUAL e1 = expr IN e2 = expr END
    { Handle (x, fname, e1, e2) }
  | e1 = expr SEMI e2 = expr
    { Seq (e1, e2) }

tm_param:
  | COMMA? x = VAR COLON t = ty
    { (x, t) }

hd_param:
  | COMMA? x = VAR COLON t = VAR
    { (HVar x, t) }

hd_arg:
  | x = VAR
    { HVar x }

eff_name:
  | COMMA? x = VAR
    { EVar x }
  | COMMA? TILDE x = VAR
    { Handler (HVar x) }

ty:
  | COMMA? TBOOL
    { TBool }
  | COMMA? TINT
    { TInt }
  | COMMA? TUNIT
    { TUnit }
  | COMMA? LPAREN t = ty RPAREN
    { t }
  | COMMA? FORALL LBRACKET es1 = eff_name* RBRACKET DOT 
    FORALL LBRACE hs = hd_param* RBRACE DOT 
    LPAREN ts = ty* RPAREN 
    TARROW t1 = ty UNDERSCORE LBRACKET es2 = eff_name* RBRACKET
    { TAbs (es1, hs, ts, t1, es2) }

effect_declare:
  | EFF x = VAR COLON t = ty
    { Decl_eff (x, t) }

mark_position(X):
  x = X
  { Zoo.locate ~loc:(Zoo.make_location $startpos $endpos) x }

%%
