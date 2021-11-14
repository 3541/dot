{ lib, config, pkgs, ... }:
let cfg = config.a3;
in {
  config = {
    environment.systemPackages = with pkgs; [ vim wget file borgbackup ];

    documentation.dev.enable = cfg.role == "workstation";
    documentation.man.generateCaches = cfg.role == "workstation";

    programs.steam.enable = cfg.role == "workstation";
  };
}
