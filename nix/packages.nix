{ lib, config, ... }:
let
  cfg = config.a3;
in
{
  homebrew = lib.mkIf (cfg.system.os == "darwin" && cfg.system.role == "workstation") {
    enable = true;

    onActivation = {
      autoUpdate = true;
      cleanup = "zap";
      upgrade = true;
    };

    casks = [
      "alacritty"
      "firefox@esr"
      "signal"
    ];
  };
}
