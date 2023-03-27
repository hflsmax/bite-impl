{
  description = "A very basic flake";

  inputs = {
    # Convenience functions for writing flakes
    flake-utils.url = "github:numtide/flake-utils";
    # Precisely filter files copied to the nix store
    nix-filter.url = "github:numtide/nix-filter";
  };

  outputs = { self, nixpkgs, flake-utils, nix-filter }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        legacyPackages = nixpkgs.legacyPackages.${system};
        ocamlPackages = legacyPackages.ocamlPackages;
        lib = legacyPackages.lib;

        # Filtered sources (prevents unecessary rebuilds)
        sources = {
          ocaml = nix-filter.lib {
            root = ./.;
            include = [
              ".ocamlformat"
              "dune-project"
              (nix-filter.lib.inDirectory "bin")
              (nix-filter.lib.inDirectory "lib")
              (nix-filter.lib.inDirectory "test")
              (nix-filter.lib.inDirectory "examples")
            ];
          };

          nix = nix-filter.lib {
            root = ./.;
            include = [
              (nix-filter.lib.matchExt "nix")
            ];
          };
        };
      in
      {
        packages = {
          #     $ nix build
          #     $ nix run -- <args?>
          default = self.packages.${system}.bite;

          bite = ocamlPackages.buildDunePackage {
            pname = "bite";
            version = "0.1.0";
            duneVersion = "3";
            src = sources.ocaml;

            buildInputs = [
              ocamlPackages.ppx_sexp_conv
              ocamlPackages.core
              ocamlPackages.ocaml-lsp
            ];

            nativeBuildInputs = [
              legacyPackages.ocamlformat
              ocamlPackages.menhir
              legacyPackages.clang_15
            ];

            # we build examples in buildPhase because that's when we have access to $out
            postBuild =
              let
                examples = builtins.map (name: lib.strings.removeSuffix ".bite" name) (builtins.filter (path: lib.strings.hasSuffix ".bite" path) (builtins.attrNames (builtins.readDir ./examples)));
              in
              (builtins.concatStringsSep ""
                ((builtins.map
                  (name:
                    ''
                      echo "checking example ${name}"
                      dune exec bite -- -l ./examples/${name}.bite -o ./examples/${name}.compiled.c
                      clang -O3 -o ./examples/${name}.exe ./examples/${name}.compiled.c
                      ./examples/${name}.exe
                    '')
                  examples) ++ ["mkdir $out; cp -r ./examples/ $out/examples"]));

          };
        };

        checks = {
          bite =
            let
              patchDuneCommand =
                let
                  subcmds = [ "build" "test" "runtest" "install" ];
                in
                lib.replaceStrings
                  (lib.lists.map (subcmd: "dune ${subcmd}") subcmds)
                  (lib.lists.map (subcmd: "dune ${subcmd} --display=short") subcmds);
            in
            self.packages.${system}.bite.overrideAttrs
              (oldAttrs: {
                name = "check-${oldAttrs.name}";
                doCheck = true;
                buildPhase = patchDuneCommand oldAttrs.buildPhase;
                checkPhase = patchDuneCommand oldAttrs.checkPhase;
                installPhase = "touch $out";
              });

          dune-fmt = legacyPackages.runCommand "check-dune-fmt"
            {
              nativeBuildInputs = [
                ocamlPackages.dune_3
                ocamlPackages.ocaml
                legacyPackages.ocamlformat
              ];
            }
            ''
              echo "checking dune and ocaml formatting"
              dune build \
                --display=short \
                --no-print-directory \
                --root="${sources.ocaml}" \
                --build-dir="$(pwd)/_build" \
                --auto-promote \
                @fmt
              touch $out
            '';
        };
      });
}
