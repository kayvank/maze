{ compiler ? "ghc901" }:

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      "maze" =
        hself.callCabal2nix
          "maze"
          (gitignore ./.)
          {};
    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: [
      p."maze"
    ];
    buildInputs = [
      myHaskellPackages.haskell-language-server
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.ghcid
      pkgs.haskellPackages.ormolu
      pkgs.haskellPackages.hlint
      pkgs.haskellPackages.hpack
      pkgs.niv
      pkgs.nixpkgs-fmt
    ];
    withHoogle = true;
    shellHooks = ''
        set -o vi
        /home/kayvan/.emacs.d/bin/doom env
    '';
  };

  exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages."maze");

  docker = pkgs.dockerTools.buildImage {
    name = "maze";
    config.Cmd = [ "${exe}/bin/maze" ];
  };
in
{
  inherit shell;
  inherit exe;
  inherit docker;
  inherit myHaskellPackages;
  "maze" = myHaskellPackages."maze";
}
