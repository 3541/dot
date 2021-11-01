{ config, pkgs, ... }: {
  xsession.windowManager.i3 = let
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
  in {
    enable = true;

    extraConfig = "new_window pixel 0";
    config = {
      terminal = "sakura";
      modifier = "Mod4";

      fonts = {
        names = [ "Iosevka" ];
        style = "Regular";
        size = 14.0;
      };

      gaps = {
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
        statusCommand = "i3blocks";
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

      startup = [
        {
          command = "feh --bg-fill /home/alex/pictures/wallpaper/current.*";
          always = true;
        }
        {
          command = "xrandr --output DP-2 --mode 3840x2160 --right-of DVI-D-0";
          always = true;
        }
      ];

      keybindings = let
        mod = config.xsession.windowManager.i3.config.modifier;
        term = config.xsession.windowManager.i3.config.terminal;
      in {
        "${mod}+Return" = "exec ${term}";
        "${mod}+semicolon" = "exec emacsclient -c";

        "${mod}+Shift+q" = "kill";
        "${mod}+d" = "exec dmenu_run -fn iosevka-14";
        "${mod}+Shift+r" = "restart";
        "${mod}+Shift+e" = ''
          exec "i3-nagbar -t warning -m 'You pressed the exit shortcut. Do you really want to exit i3? This will end your X session.' -b 'Yes, exit i3' 'i3-msg exit'"'';

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

        "${mod}+Shift+x" = "exec i3lock -c 000000";
        "${mod}+Shift+c" = ''exec "i3lock -c 000000; systemctl suspend"'';
        "${mod}+Shift+z" = ''exec "i3lock -c 000000; systemctl hibernate"'';
        "${mod}+Shift+o" = "exec xrandr --output DVI-D-0 --off";

        "${mod}+Shift+s" = ''exec "scrot -s 'pictures/screenshots/%Y-%m-%d_%H_%M_%S.png'"'';

        "XF86AudioRaiseVolume" = "exec pactl set-sink-volume 1 1%+";
        "XF86AudioLowerVolume" = "exec pactl set-sink-volume 1 1%-";
        "XF86AudioMute" = "exec pactl set-sink-mute 1 toggle";

        "XF86MonBrightnessUp" = "exec brightnessctl s 5%+";
        "XF86MonBrightnessDown" = "exec brightnessctl s 5%-";
        "Shift+XF86MonBrightnessUp" = "exec brightnessctl s 1%+";
        "Shift+XF86MonBrightnessDown" = "exec brightnessctl s 1%-";

        "XF86AudioPlay" = "exec playerctl play-pause";
        "XF86AudioPrev" = "exec playerctl previous";
        "XF86AudioNext" = "exec playerctl next";
      };
    };
  };
}
