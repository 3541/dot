{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "alex";
  home.homeDirectory = "/home/alex";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.05";

  nixpkgs.config.allowUnfree = true;
  home.packages = with pkgs; [
    signal-desktop
    discord
    thunderbird

    mathematica

    tree

    pavucontrol
    brightnessctl
    playerctl
    sysstat
    lm_sensors
    gnome.gnome-system-monitor
    linuxPackages.cpupower
    acpi

    nixfmt

    (import ./i3blocks-contrib.nix)

    (import ./me3t.nix { pkgs = pkgs; })
  ];

  home.file.sakuraConfig = {
    source = "/home/alex/dot/sakura/.config/sakura/sakura.conf";
    target = ".config/sakura/sakura.conf";
  };

  home.file.gdbinit = {
    text = "set disassembly-flavor intel";
    target = ".gdbinit";
  };

  xresources.extraConfig = builtins.readFile (pkgs.fetchFromGitHub {
    owner = "solarized";
    repo = "xresources";
    rev = "025ceddbddf55f2eb4ab40b05889148aab9699fc";
    sha256 = "0lxv37gmh38y9d3l8nbnsm1mskcv10g3i83j0kac0a2qmypv1k9f";
  } + "/Xresources.dark");

  gtk = {
    enable = true;

    theme = {
      package = pkgs.solarc-gtk-theme;
      name = "SolArc-Dark";
    };
  };

  imports = [ ./i3.nix ./emacs.nix ];

  programs.bash = {
    enable = true;
    enableVteIntegration = true;
    bashrcExtra = ''
      shopt -s globstar
      set -o vi
      export CDPATH="$CDPATH:.:/home/alex:/home/alex/src"
      if [[ ! -S $HOME/.ssh/ssh_auth_sock ]]; then
        eval $(ssh-agent)
        ln -sf "$SSH_AUTH_SOCK" "$HOME/.ssh/ssh_auth_sock"
      fi
      export SSH_AUTH_SOCK="$HOME/.ssh/ssh_auth_sock"
      export MY_GPG_KEY=0x1EECFF9EE39ED7AA
      alias jp='jq . '
      alias cpu-poke='sudo cpupower frequency-set -g powersave && sudo cpupower frequency-set -g performance'
    '';
  };

  programs.ssh = { enable = true; };

  programs.git = {
    enable = true;
    userName = "Alex O'Brien";
    userEmail = "3541ax@gmail.com";

    signing = {
      key = "0x1EECFF9EE39ED7AA";
      signByDefault = true;
    };

    delta = {
      enable = true;
      options = {
        features = "side-by-side line-numbers";
        syntax-theme = "Solarized (dark)";
        whitespace-error-style = "22 reverse";
      };
    };

    extraConfig = {
      submodule = { fetchJobs = 8; };
      pull = {
        rebase = false;
        ff = "only";
      };
      log = { showSignature = true; };
      init = { defaultBranch = "trunk"; };
    };
  };
  programs.gh.enable = true;

  services.syncthing = {
    enable = true;
    tray.enable = false;
  };

  programs.jq.enable = true;
  programs.obs-studio.enable = true;
  programs.texlive.enable = true;

  programs.firefox = {
    enable = true;
    package = pkgs.wrapFirefox (pkgs.firefox-esr-91-unwrapped.override {
      alsaSupport = false;
      waylandSupport = false;
      privacySupport = true;
      drmSupport = true;
    }) { };
  };
}
