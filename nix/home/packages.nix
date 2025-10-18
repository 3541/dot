{ pkgs, ... }:
{
  home.packages = with pkgs; [ nix-output-monitor nvd nerd-fonts.jetbrains-mono cmus ];
}
