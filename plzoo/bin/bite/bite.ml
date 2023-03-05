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
    | Syntax.Expr e ->
      (* check the type of [e], compile it, and run it. *)
      let ty = Type_check.type_of eff_defs [] [] [] e in
      (* let frm = Compile.compile e in *)
      (* let v = Machine.run frm env in *)
      Zoo.print_info "- : %t@." (Print.ty ty) ;
      eff_defs
    | Syntax.Decl_eff (x, ty) ->
      Type_check.ty_ok eff_defs [] [] ty ;
      Zoo.print_info "eff %s = %t@." x (Print.ty ty) ;
      (x, ty) :: eff_defs
end) ;;

Bite.main ()
