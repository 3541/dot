{ lib, ... }:
{
  options.a3.system = {
    role = lib.mkOption {
      type = lib.types.enum [
        "workstation"
        "server"
      ];
      default = "workstation";
    };

    os = lib.mkOption {
      type = lib.types.enum [ "darwin" ];
    };
  };

  imports = [
    ./env.nix
    ./gui
    ./nix.nix
    ./packages.nix
  ];
}
