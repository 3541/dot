{ pkgs, config, lib, ... }:
let
  cfg = config.a3;
in
{
  options.a3.nixpkgs-flake = lib.mkOption {
    type = lib.types.str;
  };
  
  config.nix = {
    optimise.automatic = true;

    settings = {
      experimental-features = "nix-command flakes";
      trusted-users = [ cfg.user.name ];
      substituters = [ "https://cache.lix.systems" ];
      trusted-public-keys = [ "cache.lix.systems:aBnZUw8zA7H35Cz2RyKFVs3H4PlGTLawyY5KRbvJR8o=" ];
      extra-nix-path = "nixpkgs=flake:${cfg.nixpkgs-flake}";
    };

    gc = {
      automatic = true;
      interval = {
        Weekday = 0;
        Hour = 0;
        Minute = 0;
      };
      options = "--delete-older-than 30d";
    };
  };
}
