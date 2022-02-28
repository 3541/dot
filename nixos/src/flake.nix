{ lib, config, pkgs, ... }:
let cfg = config.a3;
in {
  config = {
    nix = lib.mkIf cfg.flakes {
      package = pkgs.nixFlakes;
      extraOptions = ''
        experimental-features = nix-command flakes
      '';
    };
  };
}
