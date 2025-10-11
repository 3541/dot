{
  pkgs,
  lib,
  config,
  options,
  ...
}:
let
  cfg = config.a3;
in
{
  config = {
    environment.systemPackages = with pkgs; [ ripgrep ];
  }
  // lib.optionalAttrs (options ? homebrew) {
    homebrew = {
      enable = cfg.system.role == "workstation";

      onActivation = {
        autoUpdate = true;
        cleanup = "zap";
        upgrade = true;
      };

      casks = [
        "alacritty"
        "anki"
        "firefox@esr"
        "signal"
      ];
    };
  };
}
