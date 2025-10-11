{
  config,
  lib,
  options,
  pkgs,
  ...
}:
let
  cfg = config.a3;
  enable = options ? wayland;
  mod = "Mod4";
  colors = rec {
    unfocused-background = "#002b36";
    unfocused-foreground = "#839496";
    unfocused-border = "#073642";
    unfocused-active-background = "#586e75";
    unfocused-active-foreground = unfocused-border;
    focused-background = "#2aa198";
    focused-foreground-dim = unfocused-background;
    focused-foreground = "#268bd2";
    focused-border = "#fdf6e3";
    attention-background = "#dc322f";
    attention-foreground = focused-border;
  };
in
{
  options.a3.gui.sway.outputs = lib.mkOption {
    type = lib.types.attrsOf (lib.types.attrsOf lib.types.str);
    default = { };
  };

  config =
    { }
    // lib.optionalAttrs (enable) {
      wayland.windowManager.sway = {
        enable = cfg.gui.enable;
        checkConfig = false;

        extraConfig = ''
          default_border pixel 0
        '';

        config = {
          modifier = mod;
          floating.titlebar = false;
          focus.mouseWarping = "output";
          workspaceAutoBackAndForth = true;
          terminal = "alacritty";

          gaps = {
            inner = 14;
            smartGaps = true;
          };

          window = {
            hideEdgeBorders = "smart";

            commands = [
              {
                command = "title_format \"%title :: %shell \"";
                criteria.shell = ".*";
              }
            ];
          };

          input.Logitech_Logitech_Flight_Radio_Panel.events = "disabled";

          output = cfg.gui.sway.outputs // {
            "*".bg = "~/pictures/wallpaper/current.* fill";
          };

          keybindings =
            let
              emacsPackage = if (cfg.system.os == "darwin") then pkgs.emacsMacport else pkgs.emacs;
            in
            lib.mkOptionDefault {
              "Mod1+Shift+h" = "exec ${pkgs.playerctl}/bin/playerctl previous && pkill -SIGRTMIN+11 i3blocks";
              "Mod1+Shift+l" = "exec ${pkgs.playerctl}/bin/playerctl next && pkill -SIGRTMIN+11 i3blocks";
              "Mod1+Shift+space" =
                "exec ${pkgs.playerctl}/bin/playerctl play-pause && pkill -SIGRTMIN+11 i3blocks";

              "${mod}+Shift+c" = "exec swaylock -f -c 000000 && systemctl suspend";
              "${mod}+Shift+x" = "exec swaylock -f -c 000000";
              "${mod}+Shift+s" = "exec ${pkgs.grim}/bin/grim -g \"$(${pkgs.slurp}/bin/slurp)\"";
              "${mod}+d" =
                with colors;
                "exec ${pkgs.wmenu}/bin/wmenu-run -f '${cfg.gui.font.ui.family} ${toString cfg.gui.font.ui.size}' -N ${
                  builtins.substring 1 (-1) unfocused-background
                } -n ${builtins.substring 1 (-1) unfocused-foreground} -S ${
                  builtins.substring 1 (-1) focused-background
                } -s ${builtins.substring 1 (-1) focused-border} | xargs swaymsg exec --";
              "${mod}+semicolon" = "exec ${emacsPackage}/bin/emacsclient -c";

              "${mod}+Shift+m" = "move workspace to output right";

              "${mod}+Shift+o" = "exec swaymsg output HDMI-A-1 disable && swaymsg output DP-2 pos 0 0";
              "${mod}+Shift+p" = "exec swaymsg output HDMI-A-1 enable && swaymsg output DP-2 pos 1680 0";

              "Shift+XF86MonBrightnessDown" = "exec brightnessctl s 1%-";
              "Shift+XF86MonBrightnessUp" = "exec brightnessctl s 1%+";
              "XF86AudioLowerVolume" = "exec pactl set-sink-volume 0 -1% && pkill -SIGRTMIN+10 i3blocks";
              "XF86AudioMicMute" = "exec pactl set-source-mute 1 toggle";
              "XF86AudioMute" = "exec pactl set-sink-mute 0 toggle && pkill -SIGRTMIN+10 i3blocks";
              "XF86AudioNext" = "exec playerctl next && sleep 0.2 && pkill -SIGRTMIN+11 i3blocks";
              "XF86AudioPlay" = "exec playerctl play-pause";
              "XF86AudioPrev" = "exec playerctl previous && sleep 0.2 && pkill -SIGRTMIN+11 i3blocks";
              "XF86AudioRaiseVolume" = "exec pactl set-sink-volume 0 +1% && pkill -SIGRTMIN+10 i3blocks";
              "XF86MonBrightnessDown" = "exec brightnessctl s 5%-";
              "XF86MonBrightnessUp" = "exec brightnessctl s 5%+";
            };

          fonts = {
            names = [ cfg.gui.font.ui.family ];
            style = "Regular";
            size = toString cfg.gui.font.ui.size;
          };

          colors = with colors; {
            background = unfocused-background;

            focused = {
              border = unfocused-background;
              background = focused-background;
              text = focused-border;
              indicator = focused-foreground;
              childBorder = "";
            };

            focusedInactive = {
              border = unfocused-background;
              background = unfocused-border;
              text = unfocused-foreground;
              indicator = unfocused-active-foreground;
              childBorder = "";
            };

            unfocused = {
              border = unfocused-background;
              background = unfocused-border;
              text = unfocused-foreground;
              indicator = unfocused-border;
              childBorder = "";
            };

            urgent = {
              border = unfocused-background;
              background = attention-background;
              text = attention-foreground;
              indicator = unfocused-background;
              childBorder = "";
            };
          };

          bars = [
            {
              statusCommand = "${pkgs.i3blocks}/bin/i3blocks";
              position = "top";

              fonts = {
                names = [ cfg.gui.font.ui.family ];
                style = "Light";
                size = toString cfg.gui.font.ui.size;
              };

              colors = with colors; {
                background = unfocused-background;
                separator = unfocused-active-background;
                statusline = unfocused-foreground;

                focusedWorkspace = {
                  border = focused-background;
                  background = focused-background;
                  text = focused-foreground-dim;
                };

                activeWorkspace = {
                  border = unfocused-active-background;
                  background = unfocused-active-background;
                  text = unfocused-active-foreground;
                };

                inactiveWorkspace = {
                  border = unfocused-border;
                  background = unfocused-background;
                  text = unfocused-foreground;
                };

                urgentWorkspace = {
                  border = attention-background;
                  background = attention-background;
                  text = attention-foreground;
                };
              };
            }
          ];
        };
      };
    };
}
