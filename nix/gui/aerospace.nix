{
  lib,
  config,
  pkgs,
  pkgsUnstable,
  ...
}:
let
  cfg = config.a3;
in
{
  services.aerospace = lib.mkIf (cfg.system.os == "darwin") {
    enable = true;
    package = pkgsUnstable.aerospace;

    settings = {
      enable-normalization-flatten-containers = false;
      enable-normalization-opposite-orientation-for-nested-containers = false;

      workspace-to-monitor-force-assignment = {
        "1" = "main";
        "5" = "^built-in retina display$";
      };

      on-window-detected = [
        {
          check-further-callbacks = false;
          "if".app-name-regex-substring = "GuiMain";
          run = [ "layout floating" ];
        }
        {
          check-further-callbacks = false;
          "if".app-name-regex-substring = "java";
          run = [ "layout floating" ];
        }
      ];

      mode = {
        main.binding = {
          cmd-enter = "exec-and-forget open -n /Applications/Alacritty.app";

          cmd-h = "focus left";
          cmd-j = "focus down";
          cmd-k = "focus up";
          cmd-l = "focus right";

          cmd-shift-h = "move left";
          cmd-shift-j = "move down";
          cmd-shift-k = "move up";
          cmd-shift-l = "move right";

          cmd-s = "split horizontal";
          cmd-shift-v = "split vertical";

          cmd-f = "fullscreen";

          cmd-w = "layout h_accordion";
          cmd-e = "layout tiles horizontal vertical";

          cmd-shift-space = "layout floating tiling";

          cmd-shift-m = "move-workspace-to-monitor --wrap-around next";

          cmd-1 = "workspace 1";
          cmd-2 = "workspace 2";
          cmd-3 = "workspace 3";
          cmd-4 = "workspace 4";
          cmd-5 = "workspace 5";
          cmd-6 = "workspace 6";
          cmd-7 = "workspace 7";
          cmd-8 = "workspace 8";
          cmd-9 = "workspace 9";
          cmd-0 = "workspace 10";

          cmd-shift-1 = "move-node-to-workspace 1";
          cmd-shift-2 = "move-node-to-workspace 2";
          cmd-shift-3 = "move-node-to-workspace 3";
          cmd-shift-4 = "move-node-to-workspace 4";
          cmd-shift-5 = "move-node-to-workspace 5";
          cmd-shift-6 = "move-node-to-workspace 6";
          cmd-shift-7 = "move-node-to-workspace 7";
          cmd-shift-8 = "move-node-to-workspace 8";
          cmd-shift-9 = "move-node-to-workspace 9";
          cmd-shift-0 = "move-node-to-workspace 10";

          cmd-shift-c = "reload-config";

          cmd-r = "mode resize";
        };

        resize.binding = {
          h = "resize width -50";
          j = "resize height +50";
          k = "resize height -50";
          l = "resize width +50";
          enter = "mode main";
          esc = "mode main";
        };
      };
    };
  };
}
