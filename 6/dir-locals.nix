let pkgs = import <nixpkgs> { };
in pkgs.nixBufferBuilders.withPackages [ pkgs.clojure pkgs.leiningen ]
