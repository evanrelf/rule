let
  pkgs = import <nixpkgs> { };

  rule = pkgs.haskellPackages.callCabal2nix "rule" ./. { };

in
rule.env.overrideAttrs (attrs: {
  buildInputs = [
    pkgs.cabal-install
    pkgs.ghcid
  ];
})
