{ config, lib, pkgs, ... }:
let cfg = config.a3;
in {
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ vim wget file ];
    services.flatpak.enable = cfg.role == "workstation" && cfg.display.enable;
    xdg.portal = lib.mkIf (cfg.role == "workstation" && cfg.display.enable) {
      enable = true;
      extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
      wlr.enable = cfg.display.server == "wayland";
    };

    documentation = if (cfg.role == "workstation") then {
      dev.enable = true;
      man.generateCaches = true;
    } else {
      nixos.enable = false;
    };
  };
}
