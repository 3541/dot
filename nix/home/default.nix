{ cfg, ... }:
{
  programs.home-manager.enable = true;

  home = {
    username = cfg.user.name;
    homeDirectory = cfg.user.home;
  };

  imports = [
    ./alacritty.nix
    ./emacs
    ./helix.nix
    ./packages.nix
    ./shell.nix
    ./util
    ./vc.nix
  ];
}
