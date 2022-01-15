{ lib, config, pkgs, ... }:
let cfg = config.a3;
in {
  config = lib.mkIf (cfg.displayServer != "none" && cfg.platform != "macOS") {
    programs.kitty = {
      enable = true;
      font = {
        name = "Iosevka Custom";
        size = 14;
      };

      extraConfig = ''
        background              #002b36
        foreground              #839496
        cursor                  #93a1a1

        selection_background    #81908f
        selection_foreground    #002831

        color0                  #073642
        color1                  #dc322f
        color2                  #859900
        color3                  #b58900
        color4                  #268bd2
        color5                  #d33682
        color6                  #2aa198
        color7                  #eee8d5
        color9                  #cb4b16
        color8                  #002b36
        color10                 #586e75
        color11                 #657b83
        color12                 #839496
        color13                 #6c71c4
        color14                 #93a1a1
        color15                 #fdf6e3'';
    };
  };
}
