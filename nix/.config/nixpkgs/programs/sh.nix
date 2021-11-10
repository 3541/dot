{ config, pkgs, ... }: {
  home.packages = [ pkgs.linuxPackages.cpupower ];
  programs.jq.enable = true;

  home.sessionVariables = { EDITOR = "emacsclient -c"; };

  programs.bash = {
    enable = true;
    enableVteIntegration = true;
    shellAliases = {
      jp = "jq . ";
      cpu-poke =
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
    '';
  };

  programs.direnv = {
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
