{
  config = {
    a3 = {
      enable = true;
      grub = true;
      hostname = "opportunity";
      displayServer = "xorg";
      videoDrivers = [ "nvidia" ];
    };

    boot.initrd.secrets = { "keyfile" = "/etc/secrets/initrd/keyfile"; };

    boot.initrd.luks.devices."root".allowDiscards = true;
    boot.initrd.luks.devices."root".keyFile = "/keyfile";
    boot.initrd.luks.devices."boot".keyFile = "/keyfile";
    boot.initrd.luks.devices."aux".allowDiscards = true;
    boot.initrd.luks.devices."aux".keyFile = "/keyfile";
    boot.initrd.luks.devices."images".allowDiscards = true;
    boot.initrd.luks.devices."images".keyFile = "/keyfile";
    boot.initrd.luks.devices."hdd0".keyFile = "/keyfile";

    boot.initrd.luks.devices."hdd1" = {
      device = "/dev/disk/by-uuid/eee5b77a-8deb-43d8-a0ec-4cc26046dd45";
      keyFile = "/keyfile";
    };

    boot.initrd.luks.devices."hdd2" = {
      device = "/dev/disk/by-uuid/237e5dd2-c2bf-4571-895a-aa9930a8b6ef";
      keyFile = "/keyfile";
    };

    swapDevices = [{ device = "/opt/swapfile"; }];

    networking.interfaces.enp0s25.useDHCP = true;

    services.xserver.dpi = 96;
    hardware.video.hidpi.enable = true;

    services.borgbackup.jobs.backup = {
      paths = [ "/home/alex" "/etc/nixos" ];
      exclude = [
        ".cache"
        "*/Cache"
        "*/cache"
        "images"
        ".local/share/Steam"
        ".cargo"
        ".config"
        ".npm"
        "*/node_modules"
        "*/build"
        "*/venv"
        "*/.venv"
      ];
      repo = "ssh://borg@sagittarius/share/backup-marx";
      encryption = {
        passCommand = "cat /etc/nixos/borg_passphrase";
        mode = "repokey-blake2";
      };
      environment.BORG_RSH = "ssh -i /root/.ssh/id_ed25519";
      startAt = "daily";
      doInit = false;
      prune.keep = {
        daily = 10;
        weekly = 7;
        monthly = 4;
        yearly = 12;
      };
    };
  };
}
