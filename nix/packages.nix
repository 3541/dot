{ lib, config, options, ... }:
let
  cfg = config.a3;
  use-brew = options?homebrew && cfg.system.role == "workstation";
in
{
config = {} // lib.optionalAttrs(use-brew) { homebrew = {
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
  }; };
}
