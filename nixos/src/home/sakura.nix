{ cfg, lib, pkgs, ... }: {
  config.home = lib.mkIf (cfg.enable && cfg.home.enable && cfg.display.enable
    && cfg.platform != "macOS") {
      packages = [ pkgs.sakura ];

      file.sakuraConfig = {
        source = pkgs.substituteAll {
          font = "${cfg.home.ui.fonts.editor.font} ${
              toString (builtins.floor cfg.home.ui.fonts.editor.size)
            }";
          src = ./sakura.conf;
        };
        target = ".config/sakura/sakura.conf";
      };
    };
}
