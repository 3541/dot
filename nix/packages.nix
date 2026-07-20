{
  pkgs,
  lib,
  config,
  options,
  ...
}:
let
  cfg = config.a3;
  packages = with pkgs; [ ripgrep ];
in
{
  config =
    { }
    // lib.optionalAttrs (options ? environment) {
      environment.systemPackages = packages;
    }
    // lib.optionalAttrs (options ? home) {
      home.packages = packages;
    }
    // lib.optionalAttrs (options ? homebrew) {
      homebrew = {
        enable = cfg.system.role == "workstation";

        onActivation = {
          autoUpdate = true;
          cleanup = "zap";
          upgrade = true;
          extraFlags = [ "--force-cleanup" ];
        };

        brews = [ "container" ];

        casks = [
          "alacritty"
          "anki"
          "claude-code@latest"
          "firefox@esr"
          "signal"
        ];
      };
    };
}
