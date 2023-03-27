open Util

let file = ref None
let c_out_file = ref None
let ir_out_file = ref None
let wrapper = ref (Some [ "rlwrap"; "ledit" ])

let options =
  Arg.align
    [
      ( "--wrapper",
        Arg.String (fun str -> wrapper := Some [ str ]),
        "<program> Specify a command-line wrapper to be used (such as rlwrap \
         or ledit)" );
      ( "--no-wrapper",
        Arg.Unit (fun () -> wrapper := None),
        " Do not use a command-line wrapper" );
      ( "-v",
        Arg.Unit
          (fun () ->
            print_endline ("bite" ^ " " ^ "(" ^ Sys.os_type ^ ")");
            exit 0),
        " Print language information and exit" );
      ( "-l",
        Arg.String (fun str -> file := Some str),
        "<file> Load <file> into the initial environment" );
      ("-o", Arg.String (fun str -> c_out_file := Some str), "C output file");
      ("-oir", Arg.String (fun str -> ir_out_file := Some str), "IR output file");
    ]

let anonymous str =
  match !file with
  | None -> file := Some str
  | Some _ -> raise (Arg.Bad "too many files")

let read_file parser fn =
  try
    let fh = open_in fn in
    let lex = Lexing.from_channel fh in
    lex.Lexing.lex_curr_p <-
      { lex.Lexing.lex_curr_p with Lexing.pos_fname = fn };
    try
      let terms = parser lex in
      close_in fh;
      terms
    with (* Close the file in case of any parsing errors. *)
    | Error err ->
      close_in fh;
      raise (Error err)
  with
  (* Any errors when opening or closing a file are fatal. *)
  | Sys_error msg ->
    fatal_error "%s" msg

let location_of_lex lex =
  Location (Lexing.lexeme_start_p lex, Lexing.lexeme_end_p lex)

let wrap_syntax_errors parser lex =
  try parser lex with
  | Failure _ -> syntax_error ~loc:(location_of_lex lex) "unrecognised symbol"
  | _ -> syntax_error ~loc:(location_of_lex lex) "syntax error"

let compile eff_defs = function
  | Syntax.Expr ({ data = exp'; _ } as exp) ->
      (* check the type of [exp], compile it, and run it. *)
      let ((exp', _, _, _) as exp) = Type_check.type_of eff_defs [] [] [] exp in
      let ((exp', _, _, _) as exp) = Passes.wrap_in_main exp in
      let ((exp', _, _, _) as exp) = Passes.enrich_type eff_defs exp in
      let ((exp', _, _, _) as exp) = Passes.transform exp in
      let fun_infos, _ = Env_struct.get_fun_info exp None in
      let env_structs_string =
        Common.extra_defs Config.X64
        :: List.map (fun es -> Env_struct.get_env_struct es) fun_infos
      in
      let _, codes = Compile.compile exp in
      let codes = List.map Common.cleanup codes in
      ( eff_defs,
        ( Format.asprintf "%t@." (Print.rexpr' exp'),
          Printf.sprintf "%s" (String.concat "\n" env_structs_string)
          ^ String.concat "\n" codes ) )
  | Syntax.Decl_eff (x, ty) ->
      Type_check.ty_ok eff_defs [] [] ty;
      ( (x, ty) :: eff_defs,
        (Format.asprintf "eff %s = %t@." x (Print.ty ty), "") )

let use_file ctx filename =
  let cmds =
    read_file (wrap_syntax_errors (Parser.file Lexer.token)) filename
  in
  List.fold_left
    (fun (ctx, s) cmd ->
      let ctx', s' = compile ctx cmd in
      (ctx', (fst s ^ fst s', snd s ^ snd s')))
    (ctx, ("", ""))
    cmds

let main () =
  (* Intercept Ctrl-C by the user *)
  Sys.catch_break true;
  (* Parse the arguments. *)
  Arg.parse options anonymous ("Usage: " ^ "bite" ^ " [option] ... [file] ...");
  Format.set_max_boxes 42;
  Format.set_ellipsis_text "...";
  match !file with
  | None -> raise (Arg.Bad "no file specified")
  | Some file -> (
      try
        let ctx, (ir_out, c_out) = use_file [] file in
        match !c_out_file with
        | None -> Format.printf "%s" c_out
        | Some file -> (
            let oc = open_out file in
            Printf.fprintf oc "%s" c_out;
            close_out oc;
            match !ir_out_file with
            | None -> Format.printf "%s" ir_out
            | Some file ->
                let oc = open_out file in
                Printf.fprintf oc "%s" ir_out;
                close_out oc)
      with Error err ->
        print_error err;
        exit 1)

let () = main ()
