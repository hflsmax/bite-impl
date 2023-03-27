type location =
  | Location of Lexing.position * Lexing.position  (** delimited location *)
  | Nowhere  (** no location *)

type 'a located = { data : 'a; loc : location }

let sexp_of_located sexp_of_a { data; loc } = sexp_of_a data
let located_of_sexp a_of_sexp sexp = { data = a_of_sexp sexp; loc = Nowhere }
let make_location loc1 loc2 = Location (loc1, loc2)

let location_of_lex lex =
  Location (Lexing.lexeme_start_p lex, Lexing.lexeme_end_p lex)

let locate ?(loc = Nowhere) x = { data = x; loc }

exception Error of (location * string * string)

let error ?(kind = "Error") ?(loc = Nowhere) =
  let k _ =
    let msg = Format.flush_str_formatter () in
    raise (Error (loc, kind, msg))
  in
  Format.kfprintf k Format.str_formatter

(** A fatal error reported by the toplevel. *)
let fatal_error msg = error ~kind:"Fatal error" msg

(** A syntax error reported by the toplevel *)
let syntax_error ?loc msg = error ~kind:"Syntax error" ?loc msg

let print_parens ?(max_level = 9999) ?(at_level = 0) ppf =
  if max_level < at_level then (
    Format.fprintf ppf "(@[";
    Format.kfprintf (fun ppf -> Format.fprintf ppf "@])") ppf)
  else (
    Format.fprintf ppf "@[";
    Format.kfprintf (fun ppf -> Format.fprintf ppf "@]") ppf)

let print_location loc ppf =
  match loc with
  | Nowhere -> Format.fprintf ppf "unknown location"
  | Location (begin_pos, end_pos) ->
      let begin_char = begin_pos.Lexing.pos_cnum - begin_pos.Lexing.pos_bol in
      let end_char = end_pos.Lexing.pos_cnum - begin_pos.Lexing.pos_bol in
      let begin_line = begin_pos.Lexing.pos_lnum in
      let filename = begin_pos.Lexing.pos_fname in

      if String.length filename != 0 then
        Format.fprintf ppf "file %S, line %d, charaters %d-%d" filename
          begin_line begin_char end_char
      else
        Format.fprintf ppf "line %d, characters %d-%d" (begin_line - 1)
          begin_char end_char

let print_message ?(loc = Nowhere) msg_type =
  match loc with
  | Location _ ->
      Format.eprintf "%s at %t:@\n" msg_type (print_location loc);
      Format.kfprintf (fun ppf -> Format.fprintf ppf "@.") Format.err_formatter
  | Nowhere ->
      Format.eprintf "%s: " msg_type;
      Format.kfprintf (fun ppf -> Format.fprintf ppf "@.") Format.err_formatter

let print_error (loc, err_type, msg) = print_message ~loc err_type "%s" msg

let error ?(kind = "Error") ?(loc = Nowhere) =
  let k _ =
    let msg = Format.flush_str_formatter () in
    raise (Error (loc, kind, msg))
  in
  Format.kfprintf k Format.str_formatter
