{ pkgs ? import <nixpkgs> { } }:

with pkgs;

mkShell { buildInputs = [ elixir_1_10 ]; }
