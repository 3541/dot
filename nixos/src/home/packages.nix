{ cfg, lib, pkgs, ... }: {
  config = lib.mkIf (cfg.enable && cfg.home.enable) {
    home.packages = with pkgs;
      [ bashInteractive tree ripgrep pv ]
      ++ lib.optional (cfg.platform != "macOS") lm_sensors
      ++ lib.optionals (cfg.role == "workstation") [ man-pages man-pages-posix ]
      ++ lib.optionals (cfg.role == "workstation" && cfg.platform != "macOS") [
        linuxPackages.cpupower
        signal-desktop
        discord
        thunderbird
        libreoffice
        virt-manager
      ] ++ lib.optionals cfg.display.enable [
        evince
        pavucontrol
        gnome.gnome-system-monitor
      ] ++ lib.optional (cfg.display.enable && cfg.display.server == "xorg")
      scrot;

    programs = {
      ssh.enable = true;
      jq.enable = true;
      man.enable = cfg.role == "workstation";
    };

    manual.manpages.enable = cfg.role == "workstation";
    services.syncthing.enable = cfg.role == "workstation" && cfg.platform != "macOS";
  };
}
