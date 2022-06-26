{ lib, config, ... }:
let cfg = config.a3;
in {
  options.a3 = {
    enable = lib.mkEnableOption "Configuration";

    boot = lib.mkOption {
      type = lib.types.enum [ "bios" "efi" ];
      default = "efi";
    };

    bootDevice = lib.mkOption {
      type = lib.types.str;
      default = "";
    };

    luks = lib.mkOption {
      type = lib.types.bool;
      default = true;
    };

    grub = lib.mkOption {
      type = lib.types.bool;
      default = false;
    };

    esp = lib.mkOption {
      type = lib.types.path;
      default = "/boot/efi";
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

    flakes = lib.mkOption {
      type = lib.types.bool;
      default = true;
    };

    buildSshKey = lib.mkOption {
      type = lib.types.str;
      default = "id_ed25519";
    };
  };

  imports = [
    ./boot.nix
    ./build.nix
    ./display.nix
    ./flake.nix
    ./fs.nix
    ./hardware.nix
    ./l10n.nix
    ./net.nix
    ./nix.nix
    ./packages.nix
    ./services.nix
    ./workstationServices.nix
    ./workstation.nix
    ./user.nix
  ];

  config = {
    system.stateVersion = "22.05";
  };
}