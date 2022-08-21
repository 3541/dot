{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.05";

    home = {
      url = "./home";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home, ... }: {
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
          config = lib.mkIf cfg.enable { system.stateVersion = "22.05"; };
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
