{ lib, config, pkgs, ... }:
let cfg = config.a3;
in {
  config = lib.mkIf (cfg.displayServer != "none" && cfg.platform != "macOS") {
    home.packages = [ pkgs.sakura ];

    home.file.sakuraConfig = {
      source = ./sakura.conf;
      target = ".config/sakura/sakura.conf";
    };
  };
}
