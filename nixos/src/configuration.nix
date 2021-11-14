{ lib, config, ... }:
let cfg = config.a3;
in {
  options.a3 = {
    enable = lib.mkEnableOption "Configuration";

    grub = lib.mkOption {
      type = lib.types.bool;
      default = false;
    };

    smallMemory = lib.mkOption {
      type = lib.types.bool;
      default = false;
    };

    hostname = lib.mkOption { type = lib.types.str; };

    formFactor = lib.mkOption {
      type = lib.types.enum [ "stationary" "portable" ];
      default = "stationary";
    };

    role = lib.mkOption {
      type = lib.types.enum [ "workstation" "server" ];
      default = "workstation";
    };

    displayServer = lib.mkOption {
      type = lib.types.enum [ "xorg" "wayland" "none" ];
      default = "none";
    };

    videoDrivers = lib.mkOption {
      type = lib.types.listOf (lib.types.enum [ "nvidia" "intel" "nouveau" ]);
      default = [ ];
    };

    cpu = lib.mkOption {
      type = lib.types.enum [ "intel" "amd" ];
      default = "intel";
    };
  };

  imports = [
    ./boot.nix
    ./display.nix
    ./fs.nix
    ./hardened.nix
    ./hardware.nix
    ./l10n.nix
    ./net.nix
    ./nix.nix
    ./packages.nix
    ./workstationServices.nix
    ./user.nix
  ];

  config = {
    system.stateVersion = "21.05";
  };
}
