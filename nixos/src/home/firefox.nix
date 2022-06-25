{ cfg, lib, pkgs, ... }: {
  config.programs.firefox = lib.mkIf (cfg.enable && cfg.home.enable
    && cfg.display.enable && cfg.role == "workstation") (let
      override = if !cfg.potato then {
        alsaSupport = false;
        waylandSupport = cfg.display.server == "wayland";
        drmSupport = true;
        privacySupport = true;
      } else
        { };
    in {
      enable = true;

      package =
        pkgs.wrapFirefox (pkgs.firefox-esr-91-unwrapped.override override) {
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
        };
    });
}
