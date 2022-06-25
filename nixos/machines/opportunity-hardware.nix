{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [ (modulesPath + "/installer/scan/not-detected.nix")
    ];

  boot.initrd.availableKernelModules = [ "ahci" "xhci_pci" "ehci_pci" "nvme" "usb_storage" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/e4b76ff9-541a-4d48-89e1-387bd19e7f1f";
      fsType = "xfs";
    };

  boot.initrd.luks.devices."root".device = "/dev/disk/by-uuid/b3f58e71-7e0c-474b-ab8c-fcb1317a84b4";

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/9765cfaa-4d2a-44a7-9345-8fbe84163a4f";
      fsType = "ext4";
    };

  boot.initrd.luks.devices."boot".device = "/dev/disk/by-uuid/ab626a06-a41f-4bd8-a4b6-2f65fabe7ae3";

  fileSystems."/boot/efi" =
    { device = "/dev/disk/by-uuid/0723-0747";
      fsType = "vfat";
    };

  fileSystems."/opt" =
    { device = "/dev/disk/by-uuid/30e0fd91-8dab-4e6d-8902-2959b0416d96";
      fsType = "btrfs";
      options = [ "subvol=opt" ];
    };

  boot.initrd.luks.devices."aux".device = "/dev/disk/by-uuid/820f2c79-e198-44be-a15a-4a2ff07c7745";

  fileSystems."/mass" =
    { device = "/dev/mapper/hdd0";
      fsType = "btrfs";
      options = [ "subvol=mass" ];
    };

  boot.initrd.luks.devices."hdd0".device = "/dev/disk/by-uuid/b97036eb-9f6b-4e29-b563-42aacf62829e";

  fileSystems."/home/alex/images" =
    { device = "/dev/disk/by-uuid/336cbdad-eb7a-440f-bbb8-364b09255e98";
      fsType = "xfs";
    };

  boot.initrd.luks.devices."images".device = "/dev/disk/by-uuid/4fa20fd6-adf4-4e61-81af-6e4fafd5b784";

  swapDevices = [ ];

}
