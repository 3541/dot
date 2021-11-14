{ lib, config, pkgs, ... }:
let
  cfg = config.a3;
  firefoxOverride = if !cfg.potato then {
    alsaSupport = false;
    waylandSupport = cfg.displayServer != "wayland";
    drmSupport = true;
    privacySupport = true;
  } else
    { };
in {
  config = lib.mkIf (cfg.role == "workstation") {
    programs.firefox = {
      enable = true;
      package = pkgs.wrapFirefox
        (pkgs.firefox-esr-91-unwrapped.override firefoxOverride) {
          extraPolicies = {
            DisableFirefoxStudies = true;
            DisablePocket = true;
            DisableTelemetry = true;
            FirefoxHome = {
              Pocket = false;
              Snippets = false;
            };
            UserMessaging = {
              ExtensionRecommendations = false;
              SkipOnboarding = true;
            };
          };

          extraPrefs = ''
            lockPref("security.identityblock.show_extended_validation", true);
          '';
        };
    };
  };
}
