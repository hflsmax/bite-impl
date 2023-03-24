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
          ];
        };

        nix = nix-filter.lib {
          root = ./.;
          include = [
            (nix-filter.lib.matchExt "nix")
          ];
        };
      };
    in {
      packages = {
        # The package that will be built or run by default. For example:
        #
        #     $ nix build
        #     $ nix run -- <args?>
        #
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
            ocamlPackages.menhir
          ];

        };
      };
    });
}
