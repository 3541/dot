{ cfg, lib, pkgs, i3blocks-contrib, ... }: {
  config = lib.mkIf (cfg.enable && cfg.home.enable && cfg.display.enable) (let
    mod = "Mod4";
    term = "${pkgs.alacritty}/bin/alacritty";

    lockCommand = if (cfg.display.server == "wayland") then
      "${pkgs.swaylock}/bin/swaylock -f -c 000000"
    else
      "${pkgs.i3lock}/bin/i3lock -c 000000";

    dmenuArgs = " -fn '${cfg.home.ui.fonts.editor.font}-${
         toString cfg.home.ui.fonts.ui.size
       }' -nb '${colors.background}' -nf '${colors.foreground}' -sb '${colors.focus}' -sf '${colors.bright}'";

    i3b-contrib = pkgs.stdenv.mkDerivation {
      name = "i3blocks-contrib";
      src = i3blocks-contrib;
    };

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

    wmConfig = lib.mkMerge ([{
      enable = true;

      extraConfig = lib.optionalString (cfg.display.server == "wayland") ''
        for_window [shell=".*"] title_format "%title (%shell)"
      '';

      config = {
        modifier = mod;
        terminal = term;
        window.titlebar = !cfg.home.ui.windowGaps;
        workspaceAutoBackAndForth = true;

        fonts = {
          names = [ cfg.home.ui.fonts.ui.font ];
          style = "Regular";
          size = cfg.home.ui.fonts.ui.size;
        };

        gaps = lib.mkIf cfg.home.ui.windowGaps {
          smartGaps = true;
          smartBorders = "on";
          inner = 20;
          outer = 7;
        };

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
            names = [ cfg.home.ui.fonts.ui.font ];
            style = "Light";
            size = cfg.home.ui.fonts.ui.size;
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

        startup = lib.optional (cfg.display.server == "xorg") {
          command =
            "feh --bg-fill ${cfg.home.directory}/pictures/wallpaper/current.*";
          always = true;
        } ++ lib.optional (cfg.display.server == "wayland") {
          command = "mako";
        } ++ lib.optional (cfg.role == "workstation") {
          command = "opensnitch-ui";
        };

        keybindings = {
          "${mod}+Return" = "exec ${term}";
          "${mod}+semicolon" = "exec ${pkgs.emacs}/bin/emacsclient -c";

          "${mod}+Shift+q" = "kill";
          "${mod}+d" =
            "exec ${pkgs.dmenu}/bin/dmenu_path | ${pkgs.dmenu}/bin/dmenu ${dmenuArgs}"
            + (if cfg.display.server == "wayland" then
              " | xargs swaymsg exec --"
            else
              "| /bin/sh");
          "${mod}+Shift+w" = lib.mkIf (cfg.hardware.formFactor == "portable")
            ("exec ${pkgs.networkmanager_dmenu}/bin/networkmanager_dmenu ${dmenuArgs}"
              + lib.optionalString (cfg.display.server == "wayland")
              " | xargs swaymsg exec --");
          "${mod}+Shift+r" =
            if (cfg.display.server == "wayland") then "reload" else "restart";
          "${mod}+Shift+e" = if (cfg.display.server == "wayland") then
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

          "${mod}+Shift+s" = if (cfg.display.server == "wayland") then
            ''exec ${pkgs.grim}/bin/grim -g "$(${pkgs.slurp}/bin/slurp)"''
          else
            ''
              exec ${pkgs.scrot}/bin/scrot -s "pictures/screenshots/%Y-%m-%d_%H_%M_%S.png"'';

          "XF86AudioRaiseVolume" =
            "exec ${pkgs.pulseaudio}/bin/pactl set-sink-volume 0 +1% && pkill -SIGRTMIN+10 i3blocks";
          "XF86AudioLowerVolume" =
            "exec ${pkgs.pulseaudio}/bin/pactl set-sink-volume 0 -1% && pkill -SIGRTMIN+10 i3blocks";
          "XF86AudioMute" =
            "exec ${pkgs.pulseaudio}/bin/pactl set-sink-mute 0 toggle && pkill -SIGRTMIN+10 i3blocks";
          "XF86AudioMicMute" =
            "exec ${pkgs.pulseaudio}/bin/pactl set-source-mute 1 toggle";

          "XF86MonBrightnessUp" =
            "exec ${pkgs.brightnessctl}/bin/brightnessctl s 5%+";
          "XF86MonBrightnessDown" =
            "exec ${pkgs.brightnessctl}/bin/brightnessctl s 5%-";
          "Shift+XF86MonBrightnessUp" =
            "exec ${pkgs.brightnessctl}/bin/brightnessctl s 1%+";
          "Shift+XF86MonBrightnessDown" =
            "exec ${pkgs.brightnessctl}/bin/brightnessctl s 1%-";

          "XF86AudioPlay" = "exec ${pkgs.playerctl}/bin/playerctl play-pause";
          "XF86AudioPrev" =
            "exec ${pkgs.playerctl}/bin/playerctl previous && pkill -SIGRTMIN+11 i3blocks";
          "XF86AudioNext" =
            "exec ${pkgs.playerctl}/bin/playerctl next && pkill -SIGRTMIN+11 i3blocks";
          "Mod1+Shift+space" =
            "exec ${pkgs.playerctl}/bin/playerctl play-pause && pkill -SIGRTMIN+11 i3blocks";
          "Mod1+Shift+h" =
            "exec ${pkgs.playerctl}/bin/playerctl previous && pkill -SIGRTMIN+11 i3blocks";
          "Mod1+Shift+l" =
            "exec ${pkgs.playerctl}/bin/playerctl next && pkill -SIGRTMIN+11 i3blocks";
        };

        modes.resize = {
          Escape = "mode default";
          Return = "mode default";
          Down = "resize grow height 10 px or 10 ppt";
          j = "resize grow height 10 px or 10 ppt";
          Left = "resize shrink width 10 px or 10 ppt";
          h = "resize shrink width 10 px or 10 ppt";
          Right = "resize grow width 10 px or 10 ppt";
          l = "resize grow width 10 px or 10 ppt";
          Up = "resize shrink height 10 px or 10 ppt";
          k = "resize shrink height 10 px or 10 ppt";
        };
      };
    }] ++ lib.optional (cfg.display.server == "wayland") {
      wrapperFeatures.gtk = true;

      config.output."*".bg =
        "${cfg.home.directory}/pictures/wallpaper/current.* fill";

      extraSessionCommands = ''
        export SDL_VIDEODRIVER=wayland
        export QT_QPA_PLATFORM=wayland
        export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
        export _JAVA_AWT_WM_NONREPARENTING=1
        export MOZ_ENABLE_WAYLAND=1
        # Makes media keys work. Some kind of race condition bug in lightdm.
        sleep 1
      '';
    });

  in {
    xsession.windowManager.i3 =
      if cfg.display.server == "xorg" then wmConfig else { };
    wayland.windowManager.sway =
      if cfg.display.server == "wayland" then wmConfig else { };

    services.notify-osd.enable = cfg.role == "workstation" && cfg.display.server
      == "xorg";
    programs.mako =
      lib.mkIf (cfg.role == "workstation" && cfg.display.server == "wayland") {
        enable = true;
        backgroundColor = colors.background;
        borderColor = colors.focus;
        textColor = colors.foreground;
        font =
          "${cfg.home.ui.fonts.ui.font} ${toString cfg.home.ui.fonts.ui.size}";
      };

    home = {
      packages = with pkgs; [ python3 sysstat acpi ];

      file = {
        nmDmenuConfig = {
          target = ".config/networkmanager-dmenu/config.ini";
          text = ''
            [dmenu]
            dmenu_command = ${pkgs.dmenu}/bin/dmenu
          '';
        };

        i3blocksConfig = {
          text = ''
            # Global properties
            #
            # The top properties below are applied to every block, but can be overridden.
            # Each block command defaults to the script name to avoid boilerplate.
            command=${i3b-contrib}/libexec/i3blocks/$BLOCK_NAME
            separator_block_width=15
            markup=none

            [mediaplayer]
            command=echo $(${pkgs.playerctl}/bin/playerctl metadata album) - $(${pkgs.playerctl}/bin/playerctl metadata title)
            interval=5
            signal=11

            # Volume indicator
            #
            # The first parameter sets the step (and units to display)
            # The second parameter overrides the mixer selection
            # See the script for details.
            [volume]
            #label=VOL
            label=♪ 
            instance=Master
            #instance=PCM
            interval=once
            signal=10

            # Memory usage
            #
            # The type defaults to "mem" if the instance is not specified.
            [memory]
            label=MEM 
            separator=false
            interval=30

            [memory]
            label=SWAP 
            instance=swap
            interval=30

            [disk]
            label=ROOT 
            interval=30
            instance=/

            # Network interface monitoring
            #
            # If the instance is not specified, use the interface used for default route.
            # The address can be forced to IPv4 or IPv6 with -4 or -6 switches.
            [iface]
            #instance=wlan0
            color=#00FF00
            interval=10
            #separator=false

            ${lib.optionalString (cfg.hardware.formFactor == "portable") ''
              [battery2]
              interval=30
              markup=pango
            ''}

            #[bandwidth]
            #instance=eth0
            #interval=5

            # CPU usage
            #
            # The script may be called with -w and -c switches to specify thresholds,
            # see the script for details.
            [cpu_usage]
            label=CPU 
            interval=10
            #min_width=CPU: 100.00%
            #separator=false

            [load_average]
            interval=10

            [cpufreq]
            command=awk '/MHz/ { t += $4; n++ } END { print t / n }' /proc/cpuinfo
            interval=5

            # Temperature
            #
            # Support multiple chips, though lm-sensors.
            # The script may be called with -w and -c switches to specify thresholds,
            # see the script for details.
            [temperature-cpu]
            command=${i3b-contrib}/libexec/i3blocks/temperature --chip coretemp-isa-0000 -w 60 -c 80
            label=CORE 
            interval=10

            #[weather]
            #command=curl -sS 'https://wttr.in/McKinnon?format=%c+%t+%h'
            #interval=1800

            # Date Time
            #
            [time]
            command=date '+%Y-%m-%d %H:%M:%S'
            interval=1

            [time-east]
            command=echo "E: $(TZ=":America/New_York" date '+%T')"
            interval=1

            [time-pacific]
            command=echo "P: $(TZ=":America/Vancouver" date '+%T')"
            interval=1
          '';
          target = ".config/i3blocks/config";
        };
      };
    };
  });
}
