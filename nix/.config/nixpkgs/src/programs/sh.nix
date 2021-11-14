{ lib, config, pkgs, ... }:
let cfg = config.a3;
in {
  home.packages =
    lib.mkIf (cfg.role == "workstation") [ pkgs.linuxPackages.cpupower ];
  programs.jq.enable = true;

  home.sessionVariables =
    lib.mkIf (cfg.role == "workstation") { EDITOR = "emacsclient -c"; };

  programs.bash = {
    enable = true;
    enableVteIntegration = cfg.displayServer != "none";
    shellAliases = {
      jp = "jq . ";
      cpu-poke = lib.mkIf (cfg.role == "workstation")
        "sudo cpupower frequency-set -g powersave && sudo cpupower frequency-set -g performance";
    };
    initExtra = ''
      shopt -s globstar
      set -o vi
      export CDPATH="$CDPATH:.:/home/alex:/home/alex/src"
      if [[ ! -S $HOME/.ssh/ssh_auth_sock ]]; then
        eval $(ssh-agent)
        ln -sf "$SSH_AUTH_SOCK" "$HOME/.ssh/ssh_auth_sock"
      fi
      export SSH_AUTH_SOCK="$HOME/.ssh/ssh_auth_sock"
      export MY_GPG_KEY=0x1EECFF9EE39ED7AA
      export DOTNET_CLI_TELEMETRY_OPTOUT=1
    '' + cfg.shExtra + (if cfg.platform == "linux" then ". /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh" else "");
  };

  programs.direnv = lib.mkIf (cfg.role == "workstation") {
    enable = true;
    nix-direnv.enable = true;
  };

  programs.powerline-go = {
    enable = true;
    modules = [
      "user"
      "host"
      "ssh"
      "cwd"
      "git"
      "venv"
      "hg"
      "jobs"
      "root"
      "docker"
      "nix-shell"
      "node"
    ];
    settings = {
      hostname-only-if-ssh = true;
      cwd-mode = "plain";
      max-width = 25;
      theme = "solarized-dark16";
    };
  };
}
