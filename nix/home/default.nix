{ cfg, lib, ... }:
{
  programs.home-manager.enable = true;

  home = {
    username = cfg.user.name;
    homeDirectory = lib.mkForce cfg.user.home;
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
