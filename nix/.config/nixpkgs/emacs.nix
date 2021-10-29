{ config, pkgs, ... }: {
  services.emacs = {
    enable = true;
    client.enable = true;
  };

  programs.emacs = {
    enable = true;
    extraPackages = (epkgs: (with epkgs; [ emacsql-sqlite ]));
  };

  # TODO: Port this to Nix.
  home.file.emacsCfg = {
    source = "/home/alex/dot/emacs/.emacs.d";
    target = ".emacs.d";
    recursive = true;
  };

  home.file.emacsTheme = {
    source = pkgs.fetchFromGitHub {
      owner = "sellout";
      repo = "emacs-color-theme-solarized";
      rev = "f3ca8902ea056fb8e46cb09f09c96294e31cd4ee";
      sha256 = "16d7adqi07lzzr0qipl1fbag9l8kiyr3xrqxi528pimcisbg85d3";
    };
    target = ".emacs.d/emacs-color-theme-solarized/";
    recursive = true;
  };
}
