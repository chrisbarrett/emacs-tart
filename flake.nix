{
  inputs = {
    opam-nix.url = "github:tweag/opam-nix";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.follows = "opam-nix/nixpkgs";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
  };
  outputs =
    { self
    , flake-utils
    , opam-nix
    , nixpkgs
    , nixpkgs-unstable
    , ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        pkgs-unstable = nixpkgs-unstable.legacyPackages.${system};
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
            pkgs-unstable.prek
            pkgs.asciidoctor
          ];
        };
      }
    );
}
