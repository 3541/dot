{ pkgs, config, lib, ... }:
let
  cfg = config.a3;
in
{
  options.a3.nixpkgs-flake = lib.mkOption {
    type = lib.types.str;
  };

  config.nix = {
    package = pkgs.lix;
    
    settings = {
      experimental-features = "nix-command flakes";
      trusted-users = [ cfg.user.name ];
      extra-substituters = [ "https://cache.lix.systems" ];
      trusted-public-keys = [ "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" "cache.lix.systems:aBnZUw8zA7H35Cz2RyKFVs3H4PlGTLawyY5KRbvJR8o=" ];
      extra-nix-path = "nixpkgs=flake:${cfg.nixpkgs-flake}";
    };

    gc = {
      automatic = true;
      options = "--delete-older-than 30d";
    } ;
  }
// (if cfg.orchestrator == "home-manager" then { package = pkgs.lix; gc.frequency = "weekly"; } else
      {

    optimise.automatic = true;

      gc.interval = {
        Weekday = 0;
        Hour = 0;
        Minute = 0;
      };});  
}
