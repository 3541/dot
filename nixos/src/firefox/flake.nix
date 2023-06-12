{
  inputs = { nixpkgs.url = "nixpkgs/nixos-23.05"; };

  outputs = { nixpkgs, ... }:
    let pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in {
      packages.x86_64-linux.firefox = pkgs.wrapFirefox
        (pkgs.firefox-esr-102-unwrapped.override {
          privacySupport = true;
          pgoSupport = false;
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
        };
    };
}
