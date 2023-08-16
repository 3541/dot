{ cfg, lib, pkgs, ... }: {
  config.programs.alacritty = lib.mkIf (cfg.enable && cfg.home.enable
    && cfg.display.enable) {
      enable = true;

      settings = {
        cursor.style.shape = "Underline";
        window.option_as_alt = "Both";

        colors = {
          primary = {
            background = "0x002b36";
            foreground = "0x839496";
          };

          normal = {
            black = "0x073642";
            red = "0xdc322f";
            green = "0x859900";
            yellow = "0xb58900";
            blue = "0x268bd2";
            magenta = "0xd33682";
            cyan = "0x2aa198";
            white = "0xeee8d5";
          };

          bright = {
            black = "0x002b36";
            red = "0xcb4b16";
            green = "0x586e75";
            yellow = "0x657b83";
            blue = "0x839496";
            magenta = "0x6c71c4";
            cyan = "0x93a1a1";
            white = "0xfdf6e3";
          };
        };

        font = {
          normal.family = cfg.home.ui.fonts.editor.font;
          size = cfg.home.ui.fonts.editor.size;
        };

        key_bindings = lib.optionals (cfg.platform == "darwin") [
          {
            # Vertical split.
            mods = "Command";
            key = "D";
            chars = "\\x02\\x25";
          }
          {
            # Horizontal split.
            mods = "Command|Shift";
            key = "D";
            chars = "\\x02\\x22";
          }
          {
            # Close pane.
            mods = "Command";
            key = "W";
            chars = "\\x02\\x78";
          }
          {
            # Open tab.
            mods = "Command";
            key = "T";
            chars = "\\x02\\x63";
          }
          {
            # Move left.
            mods = "Command";
            key = "H";
            chars = "\\x02\\x1b\\x5b\\x44";
          }
          {
            # Move down.
            mods = "Command";
            key = "J";
            chars = "\\x02\\x1b\\x5b\\x42";
          }
          {
            # Move up.
            mods = "Command";
            key = "K";
            chars = "\\x02\\x1b\\x5b\\x41";
          }
          {
            # Move right.
            mods = "Command";
            key = "L";
            chars = "\\x02\\x1b\\x5b\\x43";
          }
          {
            # Go to tab 0.
            mods = "Command";
            key = "Key0";
            chars = "\\x02\\x30";
          }
          {
            # Go to tab 1.
            mods = "Command";
            key = "Key1";
            chars = "\\x02\\x31";
          }
          {
            # Go to tab 2.
            mods = "Command";
            key = "Key2";
            chars = "\\x02\\x32";
          }
          {
            # Go to tab 3.
            mods = "Command";
            key = "Key3";
            chars = "\\x02\\x33";
          }
          {
            # Go to tab 4.
            mods = "Command";
            key = "Key4";
            chars = "\\x02\\x34";
          }
          {
            # Go to tab 5.
            mods = "Command";
            key = "Key5";
            chars = "\\x02\\x35";
          }
          {
            # Go to tab 6.
            mods = "Command";
            key = "Key6";
            chars = "\\x02\\x36";
          }
          {
            # Go to tab 7.
            mods = "Command";
            key = "Key7";
            chars = "\\x02\\x37";
          }
          {
            # Go to tab 8.
            mods = "Command";
            key = "Key8";
            chars = "\\x02\\x38";
          }
          {
            # Go to tab 9.
            mods = "Command";
            key = "Key9";
            chars = "\\x02\\x39";
          }
        ];
      };
    };
}
