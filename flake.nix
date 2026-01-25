{
  inputs = {
    opam-nix.url = "github:tweag/opam-nix";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.follows = "opam-nix/nixpkgs";
  };
  outputs =
    { self
    , flake-utils
    , opam-nix
    , nixpkgs
    , ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        on = opam-nix.lib.${system};
        devPackagesQuery = {
          # Build
          dune = "*";
          menhir = "*";

          # Runtime deps
          ppx_deriving = "*";
          yojson = "*";

          # Testing
          alcotest = "*";

          # Dev tools
          ocaml-lsp-server = "*";
          ocamlformat = "*";
        };
        query = devPackagesQuery // {
          ocaml-base-compiler = "*";
        };
        scope = on.buildDuneProject { } "tart" self query;
        devPackages = builtins.attrValues (pkgs.lib.getAttrs (builtins.attrNames devPackagesQuery) scope);
      in
      {
        legacyPackages = scope;

        packages.default = scope.tart;

        devShells.default = pkgs.mkShell {
          inputsFrom = [ scope.tart ];
          buildInputs = devPackages ++ [
            pkgs.pre-commit
          ];
        };
      }
    );
}
