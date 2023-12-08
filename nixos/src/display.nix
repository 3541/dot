{ config, lib, pkgs, ... }:
let cfg = config.a3;
in {
  options.a3.display = {
    enable = lib.mkEnableOption "Enable a display server";

    server = lib.mkOption {
      type = lib.types.enum [ "xorg" "wayland" ];
      default = "wayland";
    };

    drivers = lib.mkOption {
      type =
        lib.types.listOf (lib.types.enum [ "amdgpu" "nvidia" "nouveau" "modesetting" ]);
      default = [ ];
    };

    hidpi = lib.mkOption {
      type = lib.types.bool;
      default = false;
    };
  };

  config = lib.mkIf (cfg.enable && cfg.display.enable) {
    services.xserver = {
      # Enabled even on Wayland configurations, since it's required for the display manager.
      enable = true;
      videoDrivers = cfg.display.drivers;
      layout = "us";
      desktopManager.xterm.enable = true;
      displayManager.defaultSession =
        if (cfg.display.server == "wayland") then "sway" else "none+i3";

      windowManager.i3 = lib.mkIf (cfg.display.server == "xorg") {
        enable = true;
        package = pkgs.i3-gaps;
        extraPackages = with pkgs; [ dmenu i3status i3lock i3blocks feh ];
      };

      libinput = {
        enable = true;
        mouse.accelProfile =
          lib.mkIf (cfg.hardware.formFactor == "fixed") "flat";
      };
    };

    programs.sway = lib.mkIf (cfg.display.server == "wayland") {
      enable = true;
      extraSessionCommands = ''
        export SDL_VIDEODRIVER=wayland
        export QT_QPA_PLATFORM=wayland
        export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
        export _JAVA_AWT_WM_NONREPARENTING=1
        export MOZ_ENABLE_WAYLAND=1
      '';
      extraPackages = [ ];
      wrapperFeatures.gtk = true;
    };

    hardware.opengl.driSupport32Bit = true;

    fonts = {
      fontconfig = lib.mkIf cfg.display.hidpi {
        antialias = true;

        subpixel = {
          rgba = "none";
          lcdfilter = "none";
        };
      };

      packages = with pkgs; [
        vistafonts
        font-awesome
        font-awesome_4
        nerdfonts
      ];
    };
  };
}
