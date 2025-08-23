{ cfg, lib, ... }:
{
  config.programs = {
    nushell = {
      enable = true;
      configFile.source = ../../home/nushell/dot-config/nushell/config.nu;
      envFile.source = ../../home/nushell/dot-config/nushell/env.nu;
      extraConfig = builtins.concatStringsSep "\n" cfg.shell.nuExtra;
    };

    zoxide = {
      enable = true;
      enableNushellIntegration = true;
    };

    direnv = lib.mkIf (cfg.system.role == "workstation") {
      enable = true;
      nix-direnv.enable = true;
      enableNushellIntegration = true;
    };

    tmux = {
      enable = true;
      clock24 = true;
      historyLimit = 50000;
      keyMode = "vi";
      escapeTime = 0;
      mouse = true;
      terminal = "tmux-direct";

      extraConfig = ''
        set -as terminal-features ",alacritty*:RGB"
      '';
    };
  };
}
