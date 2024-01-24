{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-23.11";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, utils, ... }:
    utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        packages.firefox = pkgs.wrapFirefox
          (pkgs.firefox-esr-115-unwrapped.override {
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
      });
}
