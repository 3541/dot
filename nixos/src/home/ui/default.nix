{ cfg, lib, pkgs, solarized-xresources, ... }: {
  config = lib.mkIf (cfg.enable && cfg.home.enable && cfg.display.enable) {
    xresources.extraConfig =
      builtins.readFile (solarized-xresources.outPath + "/Xresources.dark");

    gtk = {
      enable = true;
      theme = {
        package = pkgs.solarc-gtk-theme;
        name = "SolArc-Dark";
      };
    };
  };

  imports = [ ./wm.nix ];
}
