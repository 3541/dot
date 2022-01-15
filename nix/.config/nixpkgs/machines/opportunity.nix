{ pkgs, ... }: {
  imports = [ ../src/programs/me3t.nix ];

  config = {
    a3 = {
      enable = true;
      displayServer = "xorg";
      windowGaps = true;
    };

    xsession.windowManager.i3.config.startup = [{
      command =
        "xrandr --output DP-0 --mode 3840x2160 --output DVI-D-0 --mode 1680x1050 --right-of DP-0";
      always = true;
    }];
    xsession.windowManager.i3.config.keybindings."Mod4+Shift+o" =
      "exec xrandr --output DVI-D-0 --off";

    programs.obs-studio.enable = true;

    home.packages = [ pkgs.lutris ];
  };
}
