{ cfg, lib, pkgs, firefox, ... }: {
  config.programs.firefox = lib.mkIf (cfg.enable && cfg.home.enable
    && cfg.display.enable && cfg.role == "workstation") {
      enable = true;
      package = firefox.packages.${cfg.system}.firefox;
    };
}
