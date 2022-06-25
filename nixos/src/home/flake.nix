{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.05";

    home-manager = {
      url = "github:nix-community/home-manager/release-22.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    solarized-xresources = {
      url = "github:solarized/xresources";
      flake = false;
    };

    i3blocks-contrib = {
      url = "github:vivien/i3blocks-contrib";
      flake = false;
    };

    emacs-color-theme-solarized = {
      url = "github:sellout/emacs-color-theme-solarized";
      flake = false;
    };

    emacs-sensible-defaults = {
      url = "github:hrs/sensible-defaults.el";
      flake = false;
    };
  };

  outputs = { self, home-manager, solarized-xresources, i3blocks-contrib
    , emacs-color-theme-solarized, emacs-sensible-defaults, ... }: {
      nixosModules = {
        home = { config, lib, ... }@args:
          let cfg = config.a3;
          in {
            imports = [ home-manager.nixosModule ];

            options.a3.home = {
              enable = lib.mkEnableOption "Enable home-manager.";

              directory = lib.mkOption {
                type = lib.types.path;
                default = "/home/alex";
              };

              shExtra = lib.mkOption {
                type = lib.types.str;
                default = "";
              };

              ui = {
                windowGaps = lib.mkOption {
                  type = lib.types.bool;
                  default = false;
                };

                fonts = {
                  ui = {
                    font = lib.mkOption {
                      type = lib.types.str;
                      default = "Iosevka";
                    };

                    size = lib.mkOption {
                      type = lib.types.float;
                      default = 14.0;
                    };
                  };

                  editor = {
                    font = lib.mkOption {
                      type = lib.types.str;
                      default = "Iosevka Custom";
                    };

                    size = lib.mkOption {
                      type = lib.types.float;
                      default = 14.0;
                    };
                  };
                };
              };
            };

            config.home-manager = lib.mkIf (cfg.enable && cfg.home.enable) {
              useGlobalPkgs = true;
              extraSpecialArgs = {
                cfg = cfg;
                solarized-xresources = solarized-xresources;
                i3blocks-contrib = i3blocks-contrib;
                emacs-color-theme-solarized = emacs-color-theme-solarized;
                emacs-sensible-defaults = emacs-sensible-defaults;
              };

              users.alex = {
                config.home = {
                  stateVersion = "22.05";
                  username = "alex";
                  homeDirectory = cfg.home.directory;
                };

                imports = [
                  ./cmus.nix
                  ./emacs.nix
                  ./firefox.nix
                  ./gdb.nix
                  ./git.nix
                  ./packages.nix
                  ./sakura.nix
                  ./sh.nix
                  ./ui
                  ./util
                ];
              };
            };
          };

        default = self.nixosModules.home;
      };

      nixosModule = self.nixosModules.default;
    };
}
