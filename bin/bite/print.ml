open Core
open Util

let effs es ppf =
  print_parens ppf "%s" (Sexp.to_string_hum (Syntax.sexp_of_effs es))

let t_ENV t_env ppf =
  print_parens ppf "%s" (Sexp.to_string_hum (Syntax.sexp_of_t_ENV t_env))

let h_ENV h_env ppf =
  print_parens ppf "%s" (Sexp.to_string_hum (Syntax.sexp_of_h_ENV h_env))

let expr e ppf =
  print_parens ppf "%s" (Sexp.to_string_hum (Syntax.sexp_of_expr' e))

let rexpr' e ppf =
  print_parens ppf "%s" (Sexp.to_string_hum (Syntax.R.sexp_of_expr' e))

let rexpr e ppf =
  print_parens ppf "%s" (Sexp.to_string_hum (Syntax.R.sexp_of_expr e))

let ty t ppf = print_parens ppf "%s" (Sexp.to_string_hum (Syntax.sexp_of_ty t))

let static_link sl ppf =
  print_parens ppf "%s" (Sexp.to_string_hum (Syntax.sexp_of_static_link sl))

let fun_info es ppf =
  print_parens ppf "%s" (Sexp.to_string_hum (Env_struct.sexp_of_fun_info es))

let tys ts ppf =
  print_parens ppf "%s" (Sexp.to_string_hum (Syntax.sexp_of_tys ts))

let lambda_kind k ppf =
  print_parens ppf "%s" (Sexp.to_string_hum (Syntax.sexp_of_lambda_kind k))

let attrs a ppf =
  print_parens ppf "%s" (Sexp.to_string_hum (Syntax.R.sexp_of_attrs a))
