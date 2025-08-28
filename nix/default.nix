{ lib, ... }:
{
  options.a3 = {
    orchestrator = lib.mkOption {
      type = lib.types.enum ["nixos" "nix-darwin" "home-manager"];
    };
    
    system = {
      role = lib.mkOption {
        type = lib.types.enum [
          "workstation"
          "server"
        ];
        default = "workstation";
      };

      os = lib.mkOption {
        type = lib.types.enum [ "darwin" "linux" ];
      };
    };
  };

  imports = [
    ./gui
    ./home/options.nix
    ./nix.nix
    ./packages.nix
    ./user.nix
  ];
}
