open Util

let effs es ppf =
  print_parens ppf "%s" (Yojson.Safe.to_string (Syntax.yojson_of_effs es))

let t_ENV t_env ppf =
  print_parens ppf "%s" (Yojson.Safe.to_string (Syntax.yojson_of_t_ENV t_env))

let h_ENV h_env ppf =
  print_parens ppf "%s" (Yojson.Safe.to_string (Syntax.yojson_of_h_ENV h_env))

let f_ENV f_env ppf =
  print_parens ppf "%s" (Yojson.Safe.to_string (Syntax.yojson_of_f_ENV f_env))

let expr e ppf =
  print_parens ppf "%s" (Yojson.Safe.to_string (Syntax.yojson_of_expr e))

let expr' e ppf =
  print_parens ppf "%s" (Yojson.Safe.to_string (Syntax.yojson_of_expr' e))

let ty t ppf =
  print_parens ppf "%s" (Yojson.Safe.to_string (Syntax.yojson_of_ty t))

let static_link sl ppf =
  print_parens ppf "%s"
    (Yojson.Safe.to_string (Syntax.yojson_of_static_link sl))

let fun_info es ppf =
  print_parens ppf "%s"
    (Yojson.Safe.to_string (Env_struct.yojson_of_fun_info es))

let tys ts ppf =
  print_parens ppf "%s" (Yojson.Safe.to_string (Syntax.yojson_of_tys ts))

let handlerKind k ppf =
  print_parens ppf "%s" (Yojson.Safe.to_string (Syntax.yojson_of_handlerKind k))

let attrs a ppf =
  print_parens ppf "%s" (Yojson.Safe.to_string (Syntax.yojson_of_attrs a))
