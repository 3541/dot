{ config, lib, pkgs, nixpkgs-unstable, ... }:
let cfg = config.a3;
in {
  config = lib.mkIf (cfg.enable) {
    nix = {
      package = pkgs.nixFlakes;
      autoOptimiseStore = true;
      settings.trusted-users = [ "@wheel" ];

      gc = {
        automatic = true;
        dates = "weekly";
        options = "--delete-older-than 30d";
      };

      extraOptions = ''
        keep-outputs = true
        keep-derivations = true
        experimental-features = nix-command flakes
      '';
    };

    system.autoUpgrade = lib.mkIf (cfg.role == "server") {
      enable = true;
      allowReboot = true;
      flake = "/home/alex/dot/nixos";
      flags = [ "--update-input" "nixpkgs" "--commit-lock-file" ];
    };

    nixpkgs = {
      config.allowUnfree = true;

      overlays = [
        (self: super: {
          unstable = import nixpkgs-unstable {
            system = super.system;
            config.allowUnfree = true;
          };
        })
      ];
    };
  };
}
