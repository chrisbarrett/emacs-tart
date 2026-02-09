{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    { flake-utils
    , nixpkgs
    , emacs-overlay
    , ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ emacs-overlay.overlays.default ];
        };

        mkDevShell = emacs: pkgs.mkShell {
          buildInputs = [ emacs ] ++ (with pkgs; [
            # OCaml toolchain
            opam
            pkg-config

            # C deps for some OCaml packages
            gmp
            libev

            # Project tools
            asciidoctor
            prek
          ]);

          shellHook = ''
            if [ ! -f "$HOME/.opam/config" ]; then
              echo "Initializing opam..."
              opam init --bare --no-setup -y
            fi
            if [ ! -d "_opam" ]; then
              echo "Creating local opam switch..."
              opam switch create . --deps-only --with-test --with-dev-setup -y
            fi
            eval $(opam env)
          '';
        };
      in
      {
        # Emacs version matrix:
        # - emacs30: current stable (blocking in CI)
        # - emacsGit: development HEAD (advisory in CI)
        # Note: emacs29 removed from nixpkgs due to CVEs
        devShells = {
          default = mkDevShell pkgs.emacs30;
          emacs30 = mkDevShell pkgs.emacs30;
          emacsGit = mkDevShell pkgs.emacs-unstable;
        };
      }
    );
}
