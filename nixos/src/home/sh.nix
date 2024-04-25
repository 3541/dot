{ cfg, lib, pkgs, ... }: {
  config = lib.mkIf (cfg.enable && cfg.home.enable) {
    programs = {
      ssh.enable = true;
      zoxide.enable = true;

      bash = {
        enable = true;
        enableVteIntegration = cfg.display.enable && cfg.platform != "darwin";

        shellAliases = {
          jp = "jq . ";
          cpu-poke =
            lib.mkIf (cfg.role == "workstation" && cfg.platform != "darwin")
            "sudo cpupower frequency-set -g powersave && sudo cpupower frequency-set -g performance";
          b = "bazel build ...";
          t = "bazel test ...";
          r = "bazel run ...";
          g = "git";
          s = "stg";
          m = "mvnd";
          mci = "mvnd clean install";
          mi = "mvnd install";
          mt = "mvnd test";
          mda = "mvnd dependency:analyze -Dverbose";
          mvr = "mvnd versions:display-dependency-updates";
        };

        initExtra = ''
          shopt -s globstar
          set -o vi
          export CDPATH="$CDPATH:.:$HOME:$HOME/src"
          export PROMPT_COMMAND=''${PROMPT_COMMAND}'printf "\033]0;%s\007" "''${PWD/#$HOME/\~}"'
        '' + cfg.home.shExtra;
      };

      direnv = lib.mkIf (cfg.role == "workstation") {
        #enable = true;
        nix-direnv.enable = true;
      };

      nushell = {
        enable = true;
        configFile.source = ./config.nu;
        envFile.source = ./env.nu;
        extraConfig = cfg.home.nuExtra;
      };

      fzf = {
        enable = true;
        defaultCommand = "fd --type f --strip-cwd-prefix";

        colors = {
          fg = "-1";
          bg = "-1";
          hl = "#268bd2";
          "fg+" = "#eee8d5";
          "bg+" = "#073642";
          "hl+" = "#268bd2";
          info = "#b58900";
          prompt = "#b58900";
          pointer = "#fdf6e3";
          marker = "#fdf6e3";
          spinner = "#b58900";
        };
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
        escapeTime = 0;
        shell = "${pkgs.nushell}/bin/nu";
        terminal = "tmux-direct";
        historyLimit = 50000;

        extraConfig = ''
          setw -g mouse on
          set -as terminal-features ",alacritty*:RGB"
        '';
      };
    };

    home = {
      sessionVariables = {
        MY_GPG_KEY = "0x1EECFF9EE39ED7AA";
        CARGO_TARGET_DIR = "$HOME/.cache/cargo/target";
        DOTNET_CLI_TELEMETRY_OPTOUT = 1;
      };

      file.zoxideConfig = {
        source = ./zoxide.nu;
        target = ".config/zoxide/zoxide.nu";
      };
    };
  };
}
