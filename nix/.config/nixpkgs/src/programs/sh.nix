{ lib, config, pkgs, ... }:
let cfg = config.a3;
in {
  home.packages =
    lib.optional (cfg.role == "workstation" && cfg.platform != "macOS")
    pkgs.linuxPackages.cpupower;
  programs.jq.enable = true;

  home.sessionVariables =
    lib.mkIf (cfg.role == "workstation") { EDITOR = "emacsclient -c"; };

  programs.bash = {
    enable = true;
    enableVteIntegration = cfg.displayServer != "none";
    shellAliases = {
      jp = "jq . ";
      cpu-poke = lib.mkIf (cfg.role == "workstation" && cfg.platform != "macOS")
        "sudo cpupower frequency-set -g powersave && sudo cpupower frequency-set -g performance";
      b = "bazel build //...";
      t = "bazel test //...";
      r = "bazel run //...";
    };
    initExtra = ''
      shopt -s globstar
      set -o vi
      export CDPATH="$CDPATH:.:$HOME:$HOME/src"
      export MY_GPG_KEY=0x1EECFF9EE39ED7AA
      export DOTNET_CLI_TELEMETRY_OPTOUT=1
      export ALTERNATE_EDITOR=""
      export EDITOR="emacsclient -c -nw"
    '' + cfg.shExtra + (if cfg.platform == "linux" then
      ". /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh"
    else
      "");
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
      max-width = 33;
      theme = "solarized-dark16";
    };
  };

  programs.tmux = {
    enable = true;
    clock24 = true;
    newSession = true;
    keyMode = "vi";
    extraConfig = ''
      set -g mouse on
    '';
  };
}
