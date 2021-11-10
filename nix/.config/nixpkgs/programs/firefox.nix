{ config, pkgs, ... }: {
  programs.firefox = {
    enable = true;
    package = pkgs.wrapFirefox (pkgs.firefox-esr-91-unwrapped.override {
      alsaSupport = false;
      waylandSupport = false;
      privacySupport = true;
      drmSupport = true;
    }) {
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
}
