{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-23.11";

    home-manager = {
      url = "github:nix-community/home-manager/release-23.11";
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

    firefox = {
      url = "path:../firefox";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, home-manager, solarized-xresources, i3blocks-contrib
    , emacs-color-theme-solarized, emacs-sensible-defaults, firefox, nixpkgs, ... }: {
      nixosModules = {
        home = { config, lib, ... }@args:
          let cfg = config.a3;
          in {
            imports = [ home-manager.nixosModule ];

            options.a3.home = {
              enable = lib.mkEnableOption "Enable home-manager.";

              user = lib.mkOption {
                type = lib.types.str;
                default = "alex";
              };

              directory = lib.mkOption {
                type = lib.types.path;
                default = "/home/alex";
              };

              shExtra = lib.mkOption {
                type = lib.types.str;
                default = "";
              };

              nuExtra = lib.mkOption {
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
                firefox = firefox;
              };

              users.${cfg.home.user} = {
                config.home = {
                  stateVersion = "23.11";
                  username = cfg.home.user;
                  homeDirectory = cfg.home.directory;
                };

                imports = [
                  ./alacritty.nix
                  ./cmus.nix
                  ./emacs.nix
                  ./firefox.nix
                  ./gdb.nix
                  ./packages.nix
                  ./sh.nix
                  ./ui
                  ./util
                  ./vc.nix
                ];
              };
            };
          };

        default = self.nixosModules.home;
      };

      darwinModules = {
        home = { config, lib, ... }@args:
          let cfg = config.a3;
          in {
            imports = [ home-manager.darwinModules.home-manager ];

            options.a3.home = {
              enable = lib.mkEnableOption "Enable home-manager.";

              user = lib.mkOption {
                type = lib.types.str;
                default = "alex";
              };

              directory = lib.mkOption {
                type = lib.types.path;
                default = "/Users/alex";
              };

              shExtra = lib.mkOption {
                type = lib.types.str;
                default = "";
              };

              nuExtra = lib.mkOption {
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
                emacs-color-theme-solarized = emacs-color-theme-solarized;
                emacs-sensible-defaults = emacs-sensible-defaults;
              };

              users.${cfg.home.user} = {
                config.home = {
                  stateVersion = "23.11";
                  username = cfg.home.user;
                  homeDirectory = cfg.home.directory;
                };

                imports = [
                  ./alacritty.nix
                  ./cmus.nix
                  ./emacs.nix
                  ./gdb.nix
                  ./packages.nix
                  ./sh.nix
                  ./util
                  ./vc.nix
                ];
              };
            };
          };

        default = self.darwinModules.home;
      };

      homeConfigurations.aobrien-syte =
        home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.x86_64-linux;

          extraSpecialArgs = {
            emacs-color-theme-solarized = emacs-color-theme-solarized;
            emacs-sensible-defaults = emacs-sensible-defaults;
            solarized-xresources = solarized-xresources;
            i3blocks-contrib = i3blocks-contrib;

            cfg = {
              enable = true;
              role = "workstation";
              platform = "linux";
              display.enable = false;

              home = {
                enable = true;
                enableBash = false;
                shExtra = "";
                nuExtra = "";

                ui.fonts.editor = {
                  font = "Berkeley Mono";
                  size = 14.0;
                };
              };
            };
          };

          modules = [
            ({ config, pkgs, ... }@args: {
              config.home = {
                stateVersion = "23.11";
                username = "aobrien";
                homeDirectory = "/home";

                file = {
                  clangd = {
                    target = ".config/clangd/config.yaml";
                    text = ''
                      Diagnostics:
                        UnusedIncludes: Strict
                        MissingIncludes: Strict
                    '';
                  };

                  bashrc = {
                    target = ".bashrc.user";
                    text = ''
                      export LC_ALL=C.UTF-8
                      . /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
                      if [[ -z "$NU_NESTED" ]]; then
                        export NU_NESTED=0
                      fi
                      if command -v nu > /dev/null && [[ $- == *i* ]] && [[ "$NU_NESTED" -lt 1 ]]; then
                        ((++NU_NESTED))
                        exec nu
                      fi
                    '';
                  };
                };

                packages = with pkgs; [
                  pyright
                  yapf
                  cargo-flamegraph
                  black
                  black-macchiato
                  (python3.withPackages (p: with p; [ gssapi ]))
                  kubectx
                ];
              };

              imports = [
                ./emacs.nix
                ./gdb.nix
                ./packages.nix
                ./sh.nix
                ./util
                ./vc.nix
              ];
            })
          ];
        };

      nixosModule = self.nixosModules.default;
      darwinModule = self.darwinModules.default;
    };
}
