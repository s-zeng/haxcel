{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/refs/tags/22.11.tar.gz") {} }:
let

  stack-wrapped = pkgs.symlinkJoin {
    name = "stack";
    paths = [ pkgs.stack ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/stack \
        --add-flags "\
          --nix \
        "
    '';
  };

in
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [ 
    stack-wrapped
    haskell.compiler.ghc90
  ];
  # development packages
  packages = with pkgs; [
    haskell-language-server
  ];
  NIX_PATH = "nixpkgs=" + pkgs.path;
}
