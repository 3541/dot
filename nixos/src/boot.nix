{ lib, config, ... }:
let cfg = config.a3;
in {
  config = {
    boot.loader.efi.canTouchEfiVariables = cfg.boot == "efi";
    boot.loader.efi.efiSysMountPoint = lib.mkIf (cfg.boot == "efi") "/boot/efi";

    boot.loader.grub = lib.mkIf cfg.grub {
      enable = true;
      device = if (cfg.boot == "efi") then "nodev" else cfg.bootDevice;
      version = 2;
      efiSupport = cfg.boot == "efi";
      enableCryptodisk = cfg.luks;
    };
    boot.loader.systemd-boot.enable = !cfg.grub;
  };
}
