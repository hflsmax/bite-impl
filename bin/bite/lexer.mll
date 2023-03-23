{
  open Parser
}

let whitespace = [' ' '\t' '\r']
let var = ['a'-'z' 'A'-'Z']+

rule token = parse
  | whitespace { token lexbuf }
  | '\n'            { Lexing.new_line lexbuf; token lexbuf }
  | ['0'-'9']+      { INT (int_of_string(Lexing.lexeme lexbuf)) }
  | "int"           { TINT }
  | "bool"          { TBOOL }
  | "builtin"       { TBUILTIN }
  | "true"          { TRUE }
  | "false"         { FALSE }
  | "fun"           { FUN }
  | "fn"            { FN }
  | "eff"           { EFF }
  | "∀"             { FORALL }
  | "."             { DOT }
  | ","             { COMMA }
  | "_"             { UNDERSCORE }
  | "~"             { TILDE }
  | "!"             { BANG }
  | "is"            { IS }
  | "if"            { IF }
  | "then"          { THEN }
  | "else"          { ELSE }
  | "let"           { LET }
  | "dcl"           { DECL }
  | "handle"        { HANDLE }
  | "in"            { IN }
  | "end"           { END }
  | "raise"         { RAISE }
  | "resume"        { RESUME }
  | ":="            { ASSIGN }
  | ";;"            { SEMISEMI }
  | ";"             { SEMI }
  | '='             { EQUAL }
  | '<'             { LESS }
  | "->"            { ARROW }
  | ':'             { COLON }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | "()"            { UNIT }
  | '['             { LBRACKET }
  | ']'             { RBRACKET }
  | '{'             { LBRACE }
  | '}'             { RBRACE }
  | '+'             { PLUS }
  | '-'             { MINUS }
  | '*'             { TIMES }
  | var             { VAR (Lexing.lexeme lexbuf) }
  | eof             { EOF }

{
}
