open Core

let effs es ppf =
  Zoo.print_parens ppf "%s" (Sexp.to_string_hum (Syntax.sexp_of_effs es))

let t_ENV t_env ppf =
  Zoo.print_parens ppf "%s" (Sexp.to_string_hum (Syntax.sexp_of_t_ENV t_env))

let h_ENV h_env ppf =
  Zoo.print_parens ppf "%s" (Sexp.to_string_hum (Syntax.sexp_of_h_ENV h_env))

let expr e ppf =
  Zoo.print_parens ppf "%s" (Sexp.to_string_hum (Syntax.sexp_of_expr' e))

let ty t ppf =
  Zoo.print_parens ppf "%s" (Sexp.to_string_hum (Syntax.sexp_of_ty t))

let static_link sl ppf =
  Zoo.print_parens ppf "%s" (Sexp.to_string_hum (Syntax.sexp_of_static_link sl))

let fun_info es ppf =
  Zoo.print_parens ppf "%s" (Sexp.to_string_hum (Env_struct.sexp_of_fun_info es))

let tys ts ppf =
  Zoo.print_parens ppf "%s" (Sexp.to_string_hum (Syntax.sexp_of_tys ts))

(* 
let ty t ppf =
  let rec ty ?max_level t ppf =
    if not (Format.over_max_boxes ()) then
      match t with
        | Syntax.TInt -> Zoo.print_parens ppf "int"
        | Syntax.TBool -> Zoo.print_parens ppf "bool"
        | Syntax.TUnit -> Zoo.print_parens ppf "unit"
        (* | Syntax.TArrow (t1, t2) ->  *)
          (* Zoo.print_parens ppf ~at_level:1 "%t ->@ %t" (ty ~max_level:1 t1) (ty ~max_level:0 t2) *)
        (* TODO *)
        | Syntax.TAbs (es1, hs, ts, t, es2) -> 
            Zoo.print_parens ppf ~at_level:1 "∀%t.∀%t. %t -> %t_[%t]" 
              (effs es1) (h_ENV hs) (t_ENV ts) (ty ~max_level:0 t) (effs es2)
            (ty ~max_level:0 t)
        | Syntax.TMut t' -> Zoo.print_parens ppf "mut %t" (ty ~max_level:9999 t')
  in
    ty ~max_level:9999 t ppf

let mvalue m ppf =
  match m with
    | Machine.MInt k -> Zoo.print_parens ppf "%d" k
    | Machine.MBool b -> Zoo.print_parens ppf "%b" b
    | Machine.MClosure _ -> Zoo.print_parens ppf "<fun>"

   *)
