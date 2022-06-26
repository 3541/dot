{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.05";

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
              type = lib.types.enum [ "nixos" "linux" "macOS" ];
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
            ./workstation.nix
            ./user.nix
          ];
          config = lib.mkIf cfg.enable { system.stateVersion = "22.05"; };
        };

      default = self.nixosModules.system;
    };

    nixosModule = self.nixosModules.default;
  };
}
