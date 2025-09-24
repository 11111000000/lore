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
        '';
      };
    });

    checks = forAllSystems (pkgs: {
      ert = pkgs.runCommand "lore-ert" { buildInputs = [ pkgs.emacs pkgs.ripgrep ]; } ''
        cp -r ${./lisp} ./lisp
        cp -r ${./test} ./test
        EMACS="${pkgs.emacs}/bin/emacs" $EMACS -Q --batch -L lisp -l test/ert-runner.el
        touch $out
      '';
    });
  };
}
