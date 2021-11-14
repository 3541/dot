{
  imports = [ ../src/programs/me3t.nix ];

  config = {
    a3 = {
      enable = true;
      displayServer = "xorg";
      windowGaps = true;
    };

    xsession.windowManager.i3.config.startup = [{
      command = "xrandr --output DP-2 --mode 3840x2160 --right-of DVI-D-0";
      always = true;
    }];
    xsession.windowManager.i3.config.keybindings."Mod4+Shift+o" = "exec xrandr --output DVI-D-0 --off";

    programs.obs-studio.enable = true;
  };
}
