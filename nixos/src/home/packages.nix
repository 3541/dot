{ cfg, lib, pkgs, ... }: {
  config = lib.mkIf (cfg.enable && cfg.home.enable) {
    home.packages = with pkgs;
      [ bashInteractive tree ripgrep pv fd ]
      ++ lib.optional (cfg.platform != "darwin") lm_sensors
      ++ lib.optionals (cfg.role == "workstation") [ man-pages man-pages-posix ]
      ++ lib.optionals (cfg.role == "workstation" && cfg.platform != "darwin") [
        linuxPackages.cpupower
        signal-desktop
        discord
        thunderbird
        libreoffice
        virt-manager
      ] ++ lib.optionals (cfg.display.enable && cfg.platform != "darwin") [
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
    services.syncthing.enable = cfg.role == "workstation" && cfg.platform != "darwin";
  };
}
