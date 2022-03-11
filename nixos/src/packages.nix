{ lib, config, pkgs, ... }:
let cfg = config.a3;
in {
  config = {
    environment.systemPackages = with pkgs; [ vim wget file borgbackup ];
    services.flatpak.enable = cfg.role == "workstation";
    xdg.portal = lib.mkIf (cfg.role == "workstation") {
      enable = true;
      extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
      wlr.enable = cfg.displayServer == "wayland";
    };

    documentation.dev.enable = cfg.role == "workstation";
    documentation.man.generateCaches = cfg.role == "workstation";
  };
}
