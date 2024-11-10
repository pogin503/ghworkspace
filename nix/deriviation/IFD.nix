let
  drv = import ./drv.nix;
in
builtins.readFile drv.outPath
