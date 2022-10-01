{ cfg, lib, pkgs, ... }: {
  config.home =
    lib.mkIf (cfg.enable && cfg.home.enable && cfg.role == "workstation" && cfg.platform != "darwin") {
      packages = [ pkgs.cmus ];
      file.cmusConfig = {
        source = ./cmus_config;
        target = ".config/cmus/autosave";
      };
    };
}
