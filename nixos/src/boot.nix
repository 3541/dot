{ lib, config, ... }:
let cfg = config.a3;
in {
  config = {
    boot.loader.efi.canTouchEfiVariables = true;
    boot.loader.efi.efiSysMountPoint = "/boot/efi";

    boot.loader.grub = lib.mkIf cfg.grub {
      enable = true;
      device = "nodev";
      version = 2;
      efiSupport = true;
      enableCryptodisk = true;
    };
    boot.loader.systemd-boot.enable = !cfg.grub;
  };
}
