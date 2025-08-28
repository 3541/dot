{ config, lib, ... }:
let cfg = config.a3; in
{
  programs.alacritty = lib.mkIf (cfg.gui.enable) {
    enable = true;
    package = null;

    settings = {
      env.TERM = "alacritty";
      cursor.style.shape = "Underline";
      window.option_as_alt = "Both";

      font = {
        size = cfg.gui.font.text.size;
        normal.family = cfg.gui.font.text.family;
      };

      colors = {
        bright = {
          black = "0x194834";
          blue = "0x839496";
          cyan = "0x93a1a1";
          green = "0x586e75";
          magenta = "0x6c71c4";
          red = "0xcb4b16";
          white = "0xfdf6e3";
          yellow = "0x657b83";
        };

        normal = {
          black = "0x073642";
          blue = "0x268bd2";
          cyan = "0x2aa198";
          green = "0x859900";
          magenta = "0xd33682";
          red = "0xdc322f";
          white = "0xeee8d5";
          yellow = "0xb58900";
        };

        primary = {
          background = "0x002b36";
          foreground = "0x839496";
        };
      };
    };
  };
}
