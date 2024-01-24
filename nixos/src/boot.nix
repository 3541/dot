{ config, lib, ... }:
let cfg = config.a3;
in {
  options.a3.boot = {
    enable = lib.mkOption {
      default = true;
      type = lib.types.bool;
    };

    method = lib.mkOption {
      type = lib.types.enum [ "bios" "efi" "other" ];
      default = "efi";
    };

    device = lib.mkOption {
      type = lib.types.str;
      default = "";
    };

    loader = lib.mkOption {
      type = lib.types.enum [ "grub" "systemd-boot" "other" ];
      default = "systemd-boot";
    };

    esp = lib.mkOption {
      type = lib.types.path;
      default = "/boot";
    };

    canTouchEfiVariables = lib.mkOption {
      type = lib.types.bool;
      default = true;
    };
  };

  config = lib.mkIf (cfg.enable && cfg.boot.enable) {
    boot.loader.efi = lib.mkIf (cfg.boot.method == "efi") {
      canTouchEfiVariables = cfg.boot.canTouchEfiVariables;
      efiSysMountPoint = cfg.boot.esp;
    };

    boot.loader = {
      grub = lib.mkIf (cfg.boot.loader == "grub") {
        enable = true;
        device =
          if (cfg.boot.method == "efi") then "nodev" else cfg.boot.device;
        efiSupport = cfg.boot.method == "efi";
        enableCryptodisk = cfg.encryptRoot;
      };

      systemd-boot.enable = cfg.boot.loader == "systemd-boot";
    };
  };
}
