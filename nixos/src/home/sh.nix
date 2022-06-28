{ cfg, lib, pkgs, ... }: {
  config.programs = lib.mkIf (cfg.enable && cfg.home.enable) {
    bash = {
      enable = true;
      enableVteIntegration = cfg.display.enable;

      shellAliases = {
        jp = "jq . ";
        cpu-poke =
          lib.mkIf (cfg.role == "workstation" && cfg.platform != "macOS")
          "sudo cpupower frequency-set -g powersave && sudo cpupower frequency-set -g performance";
        b = "bazel build //...";
        t = "bazel test //...";
        r = "bazel run //...";
        g = "git";
        s = "stg";
      };

      initExtra = ''
        shopt -s globstar
        set -o vi
        export CDPATH="$CDPATH:.:$HOME:$HOME/src"
        export MY_GPG_KEY=0x1EECFF9EE39ED7AA
        export DOTNET_CLI_TELEMETRY_OPTOUT=1
        export ALTERNATE_EDITOR=""
        export EDITOR="emacsclient -c -nw"
        export PROMPT_COMMAND=''${PROMPT_COMMAND}'printf "\033]0;%s\007" "''${PWD/#$HOME/\~}"'
      '' + cfg.home.shExtra;
    };

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    powerline-go = {
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
        max-width = 33;
        theme = "solarized-dark16";
      };
    };

    tmux = {
      enable = true;
      clock24 = true;
      newSession = true;
      keyMode = "vi";
      extraConfig = ''
        set -g mouse on
      '';
    };
  };
}
