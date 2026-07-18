{ cfg, pkgs, ... }:
{
  home.packages =
    with pkgs;
    (
      [
        nix-output-monitor
        nvd
        nh
      ]
      ++ lib.optionals (cfg.system.role == "workstation") [
        cachix
        cmus
        nerd-fonts.jetbrains-mono
        nixd
        vagrant
      ]
      ++ lib.optionals (cfg.system.os == "darwin") [ cacert ]
    );
}
