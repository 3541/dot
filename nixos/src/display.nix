{ lib, config, pkgs, ... }:
let cfg = config.a3;
in {
  config = if (cfg.displayServer != "none") then {
    services.xserver = {
      enable = true;
      videoDrivers = cfg.videoDrivers;
      layout = "us";
      desktopManager.xterm.enable = true;
      displayManager.defaultSession =
        if (cfg.displayServer == "wayland") then "sway" else "none+i3";

      windowManager.i3 = lib.mkIf (cfg.displayServer == "xorg") {
        enable = true;
        package = pkgs.i3-gaps;
        extraPackages = with pkgs; [ dmenu i3status i3lock i3blocks feh ];
      };

      libinput = {
        enable = true;
        mouse.accelProfile = lib.mkIf (cfg.formFactor == "stationary") "flat";
      };
    };

    programs.sway = lib.mkIf (cfg.displayServer == "wayland") {
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
    fonts.fonts = with pkgs; [
      (iosevka.override {
        privateBuildPlan = {
          family = "Iosevka Custom";
          spacing = "normal";
          serifs = "slab";
          no-cv-ss = true;
          variants.design = {
            percent = "dots";
            lig-ltgteq = "slanted";
          };
          ligations = {
            inherits = "dlig";
            disables =
              [ "brace-bar" "brack-bar" "connected-tilde-as-wave" "plusplus" ];
          };
        };
        set = "custom";
      })
      vistafonts
      font-awesome
      font-awesome_4
      nerdfonts
    ];
  } else {
    environment.noXlibs = true;
  };
}
