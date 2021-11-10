{ config, pkgs, ... }: {
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  manual.manpages.enable = true;

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
    scrot
    ripgrep
    evince

    man-pages
    man-pages-posix
    python3

    pavucontrol
    brightnessctl
    playerctl
    sysstat
    lm_sensors
    gnome.gnome-system-monitor
    acpi
  ];

  xdg.mimeApps.enable = true;

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

  imports = [
    ./programs/sakura.nix
    ./programs/gdb.nix
    ./programs/sh.nix
    ./programs/git.nix
    ./programs/emacs.nix
    ./programs/firefox.nix
    ./i3.nix
  ];

  programs.ssh.enable = true;

  services.syncthing = {
    enable = true;
    tray.enable = false;
  };

  #  services.notify-osd.enable = true;

  programs.obs-studio.enable = true;
}
