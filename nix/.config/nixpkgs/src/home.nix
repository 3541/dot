{ lib, config, pkgs, ... }:
let cfg = config.a3;
in {
  options.a3 = {
    enable = lib.mkEnableOption "Configuration";

    role = lib.mkOption {
      type = lib.types.enum [ "workstation" "server" ];
      default = "workstation";
    };

    platform = lib.mkOption {
      type = lib.types.enum [ "nixos" "linux" "macOS" ];
      default = "nixos";
    };

    formFactor = lib.mkOption {
      type = lib.types.enum [ "stationary" "portable" ];
      default = "stationary";
    };

    displayServer = lib.mkOption {
      type = lib.types.enum ([ "none" ]
        ++ lib.optionals (cfg.platform != "macos") [ "xorg" "wayland" ]);
      default = "none";
    };

    potato = lib.mkOption {
      type = lib.types.bool;
      default = false;
    };

    userName = lib.mkOption {
      type = lib.types.str;
      default = "alex";
    };

    windowGaps = lib.mkOption {
      type = lib.types.bool;
      default = false;
    };

    uiFont = lib.mkOption {
      type = lib.types.str;
      default = "Iosevka";
    };

    fontSize = lib.mkOption {
      type = lib.types.float;
      default = 14.0;
    };

    editorFont = lib.mkOption {
      type = lib.types.str;
      default = "Iosevka Custom";
    };

    editorFontSize = lib.mkOption {
      type = lib.types.float;
      default = 14.0;
    };

    homeDirectory = lib.mkOption {
      type = lib.types.path;
      default = if (cfg.platform != "macOS") then
        "/home/${cfg.userName}"
      else
        "/Users/${cfg.userName}";
    };

    shExtra = lib.mkOption {
      type = lib.types.str;
      default = "";
    };
  };

  imports = [ ./programs ./wm.nix ];

  config = lib.mkIf cfg.enable {
    programs.home-manager.enable = true;
    home.stateVersion = "22.05";

    nixpkgs.config.allowUnfree = true;
    home = {
      packages = with pkgs;
        [
          bashInteractive # Listed here explicitly so it can be used as a login shell if needed.
          tree
          ripgrep
          pv
        ] ++ lib.optional (cfg.platform != "macOS") lm_sensors
        ++ lib.optionals (cfg.role == "workstation") [
          man-pages
          man-pages-posix
        ] ++ lib.optionals
        (cfg.role == "workstation" && cfg.platform != "macOS") [
          signal-desktop
          slack
          discord
          thunderbird
          zoom-us
          libreoffice
          virt-manager
        ] ++ lib.optionals (cfg.displayServer != "none") [
          evince
          pavucontrol
          gnome.gnome-system-monitor
        ] ++ lib.optional (cfg.displayServer == "xorg") scrot;
      username = cfg.userName;
      homeDirectory = cfg.homeDirectory;
    };

    programs.ssh.enable = true;
    manual.manpages.enable = cfg.role == "workstation";
    programs.man.enable = cfg.role == "workstation";
    xdg.mimeApps.enable = cfg.displayServer != "none";

    xresources.extraConfig = lib.mkIf (cfg.displayServer != "none")
      (builtins.readFile (pkgs.fetchFromGitHub {
        owner = "solarized";
        repo = "xresources";
        rev = "025ceddbddf55f2eb4ab40b05889148aab9699fc";
        sha256 = "0lxv37gmh38y9d3l8nbnsm1mskcv10g3i83j0kac0a2qmypv1k9f";
      } + "/Xresources.dark"));

    gtk = lib.mkIf (cfg.displayServer != "none") {
      enable = true;
      theme = {
        package = pkgs.solarc-gtk-theme;
        name = "SolArc-Dark";
      };
    };

    services.syncthing.enable = cfg.role == "workstation" && cfg.platform
      != "macOS";
  };
}
