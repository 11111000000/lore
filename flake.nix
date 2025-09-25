{
  description = "Lore.el - run ERT tests in batch";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
  };

  outputs = { self, nixpkgs }:
  let
    systems = [ "x86_64-linux" "aarch64-linux" ];
    forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f (import nixpkgs { inherit system; }));
  in
  {
    devShells = forAllSystems (pkgs: {
      default = pkgs.mkShell {
        packages = with pkgs; [ emacs ripgrep ];
        shellHook = ''
          echo "Run tests: emacs -Q --batch -L lisp -l test/ert-runner.el"
          echo "Or via flake app: nix run .#tests"
        '';
      };
    });

    # nix run .#tests
    apps = forAllSystems (pkgs:
      let
        emacs = pkgs.emacs;
        drv = pkgs.writeShellApplication {
          name = "lore-tests";
          # rg may be used by tests/getters
          runtimeInputs = [ pkgs.ripgrep ];
          text = ''
            set -euo pipefail
            exec ${emacs}/bin/emacs -Q --batch -L ${./lisp} -l ${./test}/ert-runner.el
          '';
        };
      in rec {
        tests = { type = "app"; program = "${drv}/bin/lore-tests"; };
        default = tests;
      });

    checks = forAllSystems (pkgs: {
      ert = pkgs.runCommand "lore-ert" { buildInputs = [ pkgs.emacs pkgs.ripgrep ]; } ''
        cp -r ${./lisp} ./lisp
        cp -r ${./test} ./test
        ${pkgs.emacs}/bin/emacs -Q --batch -L lisp -l test/ert-runner.el
        touch $out
      '';
    });
  };
}
