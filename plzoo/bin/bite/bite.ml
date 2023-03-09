module Bite = Zoo.Main (struct
  let name = "Bite"

  type command = Syntax.command

  (** A context describing the types of effects. *)
  type eff_defs = (Syntax.name * Syntax.ty) list

  type environment = eff_defs

  let options = []

  let initial_environment = ([])

  let file_parser = Some (Parser.file Lexer.token)

  let toplevel_parser = Some (Parser.toplevel Lexer.token)

  (** [exec (ctx, env) cmd] executes the toplevel command [cmd] and returns
      the new context-environment pair and a string representing the result of
      evaluation. *)
  let exec (eff_defs) = function
    | Syntax.Expr exp ->
      (* check the type of [exp], compile it, and run it. *)
      let ty, es = Type_check.type_of eff_defs [] [] [] exp in
      let exp = Common.wrap_in_main exp in
      let exp = Enrich_type.record_fname_ty eff_defs exp in
      let exp = Enrich_type.record_depth exp in
      let fun_infos = Env_struct.get_fun_info exp "DUMM" in
      let env_structs_string = List.map (fun es -> Env_struct.get_env_and_closure_decl es) fun_infos in
      let top_level_code, codes = Compile.compile exp in
      (* Zoo.print_info "env_structs:@.%s@." (String.concat "\n" env_structs_string); *)
      (* Zoo.print_info "- : %t_%t=@.%t@." (Print.ty ty) (Print.effs es) (Print.expr exp.data); *)
      (* Zoo.print_info "top_level_code:@.%s@." top_level_code; *)
      List.iter (fun code -> Zoo.print_info "code:@.%s@." code) codes;
      eff_defs
    | Syntax.Decl_eff (x, ty) ->
      Type_check.ty_ok eff_defs [] [] ty ;
      Zoo.print_info "eff %s = %t@." x (Print.ty ty) ;
      (x, ty) :: eff_defs
end) ;;

Bite.main ()
