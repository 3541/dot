{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-23.11";

    home = {
      url = "path:./home";

      inputs = {
        nixpkgs.follows = "nixpkgs";
        firefox.follows = "firefox";
      };
    };

    firefox = {
      url = "path:./firefox";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home, firefox, ... }: {
    nixosModules = {
      system = { config, lib, ... }:
        let cfg = config.a3;
        in {
          options.a3 = {
            enable = lib.mkEnableOption "Enable preset configuration.";

            hostName = lib.mkOption { type = lib.types.str; };

            role = lib.mkOption {
              type = lib.types.enum [ "workstation" "server" ];
              default = "workstation";
            };

            encryptRoot = lib.mkOption {
              type = lib.types.bool;
              default = true;
            };

            platform = lib.mkOption {
              type = lib.types.enum [ "nixos" "linux" "darwin" ];
              default = "nixos";
            };

            system = lib.mkOption {
              type = lib.types.enum [ "x86_64-linux" "aarch64-linux" ];
              default = "x86_64-linux";
            };
          };

          imports = [
            ./backup.nix
            ./boot.nix
            ./build.nix
            ./display.nix
            ./fs.nix
            ./hardware.nix
            home.nixosModule
            ./l10n.nix
            ./net.nix
            ./nix.nix
            ./packages.nix
            ./ssh.nix
            ./user.nix
            ./workstation.nix
          ];
          config = lib.mkIf cfg.enable {
            system.stateVersion = "23.11";

            # I'm sure this will be fine.
            _module.args.firefox = firefox;
          };
        };

      default = self.nixosModules.system;
    };

    darwinModules = {
      system = { config, lib, ... }:
        let cfg = config.a3;
        in {
          options.a3 = {
            enable = lib.mkEnableOption "Enable preset configuration.";

            hostName = lib.mkOption { type = lib.types.str; };

            role = lib.mkOption {
              type = lib.types.enum [ "workstation" "server" ];
              default = "workstation";
            };

            encryptRoot = lib.mkOption {
              type = lib.types.bool;
              default = true;
            };

            platform = lib.mkOption {
              type = lib.types.enum [ "nixos" "linux" "darwin" ];
              default = "darwin";
            };

            hardware.formFactor = lib.mkOption {
              type = lib.types.enum [ "fixed" "portable" ];
              default = "fixed";
            };

            display = {
              enable = lib.mkOption {
                type = lib.types.bool;
                default = true;
              };

              server = lib.mkOption {
                type = lib.types.enum [ "quartz" ];
                default = "quartz";
              };
            };
          };

          imports = [ home.darwinModule ];

          config = {
            services.nix-daemon.enable = true;
            system.stateVersion = 4;

            nix.extraOptions = "experimental-features = nix-command flakes";
          };
        };

      default = self.darwinModules.system;
    };

    nixosModule = self.nixosModules.default;
    darwinModule = self.darwinModules.default;
  };
}
