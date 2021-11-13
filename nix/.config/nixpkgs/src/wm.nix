{ lib, config, pkgs, ... }:
let cfg = config.a3;
in {
  config = lib.mkIf (cfg.displayServer != "none") (let
    colors = {
      background = "#002b36";
      foreground = "#839496";
      textbg = "#586e75";
      textfg = "#073642";
      focus = "#2aa198";
      urgent = "#dc322f";
      bright = "#fdf6e3";
      blue = "#268bd2";
    };
    mod = "Mod4";
    term = "${pkgs.sakura}/bin/sakura";
    lockCommand = if (cfg.displayServer == "wayland") then
      "swaylock -f -c 000000"
    else
      "i3lock -c 000000";
    wmConfig = {
      enable = true;

      extraConfig = lib.mkIf cfg.windowGaps "default_border none";
      config = {
        modifier = mod;
        terminal = term;

        fonts = {
          names = [ "Iosevka" ];
          style = "Regular";
          size = cfg.fontSize;
        };

        gaps = lib.mkIf cfg.windowGaps {
          smartBorders = "on";
          smartGaps = true;
          inner = 20;
          outer = 7;
        };

        workspaceAutoBackAndForth = true;

        colors = {
          background = colors.background;
          focused = {
            border = colors.background;
            background = colors.focus;
            text = colors.bright;
            indicator = colors.blue;
            childBorder = "";
          };
          focusedInactive = {
            border = colors.background;
            background = colors.textfg;
            text = colors.foreground;
            indicator = colors.textfg;
            childBorder = "";
          };
          unfocused = {
            border = colors.background;
            background = colors.textfg;
            text = colors.foreground;
            indicator = colors.textfg;
            childBorder = "";
          };
          urgent = {
            border = colors.background;
            background = colors.urgent;
            text = colors.bright;
            indicator = colors.background;
            childBorder = "";
          };
        };

        bars = [{
          statusCommand = "${pkgs.i3blocks}/bin/i3blocks";
          position = "top";
          fonts = {
            names = [ "Iosevka" ];
            style = "Light";
            size = 14.0;
          };
          colors = {
            statusline = colors.foreground;
            background = colors.background;
            separator = colors.textbg;
            focusedWorkspace = {
              border = colors.focus;
              background = colors.focus;
              text = colors.background;
            };
            activeWorkspace = {
              border = colors.textbg;
              background = colors.textbg;
              text = colors.textfg;
            };
            inactiveWorkspace = {
              border = colors.textfg;
              background = colors.background;
              text = colors.foreground;
            };
            urgentWorkspace = {
              border = colors.urgent;
              background = colors.urgent;
              text = colors.bright;
            };
          };
        }];

        startup = lib.optional (cfg.displayServer == "xorg") {
          command =
            "feh --bg-fill ${cfg.homeDirectory}/pictures/wallpaper/current.*";
          always = true;
        };

        keybindings = {
          "${mod}+Return" = "exec ${term}";
          "${mod}+semicolon" = "exec emacsclient -c";

          "${mod}+Shift+q" = "kill";
          "${mod}+d" =
            "exec ${pkgs.dmenu}/bin/dmenu_path | ${pkgs.dmenu}/bin/dmenu_run -fn iosevka-${toString cfg.fontSize}"
            + (if (cfg.displayServer == "wayland") then
              " | xargs swaymsg exec --"
            else
              "");
          "${mod}+Shift+r" =
            if (cfg.displayServer == "wayland") then "reload" else "restart";
          "${mod}+Shift+e" = if (cfg.displayServer == "wayland") then
            "exec swaynag -t warning -m 'You pressed the exit shortcut. Do you really want to exit sway? This will end your Wayland session.' -b 'Yes, exit sway' 'swaymsg exit'"
          else
            "exec i3-nagbar -t warning -m 'You pressed the exit shortcut. Do you really want to exit i3? This will end your X session.' -b 'Yes, exit i3' 'i3-msg exit'";

          "${mod}+h" = "focus left";
          "${mod}+j" = "focus down";
          "${mod}+k" = "focus up";
          "${mod}+l" = "focus right";

          "${mod}+Left" = "focus left";
          "${mod}+Down" = "focus down";
          "${mod}+Up" = "focus up";
          "${mod}+Right" = "focus right";

          "${mod}+Shift+h" = "move left";
          "${mod}+Shift+j" = "move down";
          "${mod}+Shift+k" = "move up";
          "${mod}+Shift+l" = "move right";

          "${mod}+Shift+Left" = "move left";
          "${mod}+Shift+Down" = "move down";
          "${mod}+Shift+Up" = "move up";
          "${mod}+Shift+Right" = "move right";

          "${mod}+r" = ''mode "resize"'';

          "${mod}+1" = "workspace 1";
          "${mod}+2" = "workspace 2";
          "${mod}+3" = "workspace 3";
          "${mod}+4" = "workspace 4";
          "${mod}+5" = "workspace 5";
          "${mod}+6" = "workspace 6";
          "${mod}+7" = "workspace 7";
          "${mod}+8" = "workspace 8";
          "${mod}+9" = "workspace 9";
          "${mod}+0" = "workspace 10";

          "${mod}+Shift+1" = "move container to workspace 1";
          "${mod}+Shift+2" = "move container to workspace 2";
          "${mod}+Shift+3" = "move container to workspace 3";
          "${mod}+Shift+4" = "move container to workspace 4";
          "${mod}+Shift+5" = "move container to workspace 5";
          "${mod}+Shift+6" = "move container to workspace 6";
          "${mod}+Shift+7" = "move container to workspace 7";
          "${mod}+Shift+8" = "move container to workspace 8";
          "${mod}+Shift+9" = "move container to workspace 9";
          "${mod}+Shift+0" = "move container to workspace 10";

          "${mod}+Shift+m" = "move workspace to output right";

          "${mod}+s" = "splith";
          "${mod}+v" = "splitv";

          "${mod}+w" = "layout tabbed";
          "${mod}+e" = "layout toggle split";
          "${mod}+f" = "fullscreen";
          "${mod}+Shift+space" = "floating toggle";
          "${mod}+space" = "focus mode_toggle";
          "${mod}+a" = "focus parent";

          "${mod}+Shift+x" = "exec ${lockCommand}";
          "${mod}+Shift+c" = ''exec "${lockCommand} && systemctl suspend"'';
          "${mod}+Shift+z" = ''exec "${lockCommand} && systemctl hibernate"'';

          "${mod}+Shift+s" = if (cfg.displayServer == "wayland") then
            ''exec ${pkgs.grim}/bin/grim -g "$(${pkgs.slurp}/bin/slurp)"''
          else
            ''
              exec ${pkgs.scrot}/bin/scrot -s "pictures/screenshots/%Y-%m-%d_%H_%M_%S.png"'';

          "XF86AudioRaiseVolume" =
            "exec ${pkgs.pulseaudio}/bin/pactl set-sink-volume 1 1%+";
          "XF86AudioLowerVolume" =
            "exec ${pkgs.pulseaudio}/bin/pactl set-sink-volume 1 1%-";
          "XF86AudioMute" =
            "exec ${pkgs.pulseaudio}/bin/pactl set-sink-mute 1 toggle";

          "XF86MonBrightnessUp" =
            "exec ${pkgs.brightnessctl}/bin/brightnessctl s 5%+";
          "XF86MonBrightnessDown" =
            "exec ${pkgs.brightnessctl}/bin/brightnessctl s 5%-";
          "Shift+XF86MonBrightnessUp" =
            "exec ${pkgs.brightnessctl}/bin/brightnessctl s 1%+";
          "Shift+XF86MonBrightnessDown" =
            "exec ${pkgs.brightnessctl}/bin/brightnessctl s 1%-";

          "XF86AudioPlay" = "exec ${pkgs.playerctl}/bin/playerctl play-pause";
          "XF86AudioPrev" = "exec ${pkgs.playerctl}/bin/playerctl previous";
          "XF86AudioNext" = "exec ${pkgs.playerctl}/bin/playerctl next";
        };
      } // lib.attrsets.optionalAttrs (cfg.displayServer == "wayland") {
        "*".bg = "${cfg.homeDirectory}/pictures/wallpaper/current.*";
      };
    };
  in {
    home.packages = with pkgs; [
      (import ./programs/i3blocks-contrib.nix)
      python3
      sysstat
      acpi
    ];

    xsession.windowManager.i3 = lib.mkIf (cfg.displayServer == "xorg") wmConfig;
    wayland.windowManager.sway =
      lib.mkIf (cfg.displayServer == "wayland") wmConfig;
  });
}
