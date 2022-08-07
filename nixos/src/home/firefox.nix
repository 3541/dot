{ cfg, lib, pkgs, ... }: {
  config.programs.firefox = lib.mkIf (cfg.enable && cfg.home.enable
    && cfg.display.enable && cfg.role == "workstation") (let
      override = {
        alsaSupport = false;
        waylandSupport = true;
        drmSupport = true;
        privacySupport = true;
      };
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
