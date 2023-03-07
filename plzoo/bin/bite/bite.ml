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
      let record_depth_exp = Record_depth.record_depth eff_defs exp in
      let env_structs = Env_struct.get_env_struct eff_defs exp in
      let env_structs_ast = Env_struct.env_structs_to_ast env_structs in
      Format.printf "%a@." Clang.Printer.translation_unit env_structs_ast;
      (* let frm = Compile.compile e in *)
      (* let v = Machine.run frm env in *)
      Zoo.print_info "- : %t_%t=@.%t@." (Print.ty ty) (Print.effs es) (Print.expr record_depth_exp.data);
      List.iter (fun es -> Zoo.print_info "env_structs:@.%t@." (Print.env_struct es)) env_structs;
      eff_defs
    | Syntax.Decl_eff (x, ty) ->
      Type_check.ty_ok eff_defs [] [] ty ;
      Zoo.print_info "eff %s = %t@." x (Print.ty ty) ;
      (x, ty) :: eff_defs
end) ;;

Bite.main ()
