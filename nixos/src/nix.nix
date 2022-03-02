{ lib, config, ... }:
let cfg = config.a3;
in {
  config = {
    nix = {
      autoOptimiseStore = true;
      gc = {
        automatic = true;
        dates = "weekly";
        options = "--delete-older-than 30d";
      };
      extraOptions = ''
        keep-outputs = true
        keep-derivations = true
      '';
    };

    system.autoUpgrade = lib.mkIf (cfg.role == "server") {
      enable = true;
      allowReboot = true;
    };

    nixpkgs.config.allowUnfree = true;
  };
}
