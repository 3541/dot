{ cfg, pkgs, ... }:
{
  home.packages =
    with pkgs;
    (
      [
        nix-output-monitor
        nvd
      ]
      ++ lib.optionals (cfg.system.role == "workstation") [
        nerd-fonts.jetbrains-mono
        cmus
        vagrant
        cachix
        nixd
      ]
      ++ lib.optionals (cfg.system.os == "darwin") [ cacert ]
    );
}
