{ config, pkgs, ... }: {
  programs.firefox = {
    enable = true;
    package = pkgs.wrapFirefox (pkgs.firefox-esr-91-unwrapped.override {
      alsaSupport = false;
      waylandSupport = false;
      privacySupport = true;
      drmSupport = true;
    }) { };
  };
}
