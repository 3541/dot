{ lib, config, pkgs, ... }:
let cfg = config.a3;
in {
  config = lib.mkIf (cfg.displayServer != "none" && cfg.platform != "macOS") {
    home.packages = [ pkgs.sakura ];

    home.file.sakuraConfig = {
      source = pkgs.substituteAll {
        font = "${cfg.editorFont} ${toString cfg.fontSize}";
        src = ./sakura.conf;
      };
      target = ".config/sakura/sakura.conf";
    };
  };
}
