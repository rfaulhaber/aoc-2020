let pkgs = import <nixpkgs> { };
in pkgs.nixBufferBuilders.withPackages [
  pkgs.ghc
  pkgs.haskellPackages.brittany
]
