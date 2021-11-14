{ lib, config, pkgs, ... }:
let cfg = config.a3;
in {
  config = lib.mkIf (cfg.displayServer != "none") {
    services.xserver = lib.mkIf (cfg.displayServer == "xorg") {
      enable = true;
      videoDrivers = cfg.videoDrivers;
      layout = "us";
      desktopManager.xterm.enable = true;
      displayManager.defaultSession = "none+i3";

      windowManager.i3 = {
        enable = true;
        package = pkgs.i3-gaps;
        extraPackages = with pkgs; [ dmenu i3status i3lock i3blocks feh ];
      };

      libinput = {
        enable = true;
        mouse.accelProfile = "flat";
      };
    };

    hardware.opengl.driSupport32Bit = true;
    fonts.fonts = with pkgs; [ iosevka vistafonts ];
  };
}
