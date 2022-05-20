{
  config = {
    a3 = {
      enable = true;
      grub = true;
      hostname = "opportunity";
      displayServer = "xorg";
      videoDrivers = [ "nvidia" ];
      flakes = true;
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

    services.xserver.dpi = 96;
    hardware.video.hidpi.enable = true;

    services.borgbackup.jobs.backup = {
      paths = [ "/home/alex" "/etc/nixos" ];
      exclude = [
        "/home/alex/.cache"
        "*/Cache"
        "*/cache"
        "/home/alex/images"
        "/home/alex/.local/share/Steam"
        "/home/alex/.cargo"
        "/home/alex/.config"
        "/home/alex/.npm"
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

    programs.steam.enable = true;

    services.udev.extraRules = ''
      SUBSYSTEMS=="usb", ATTRS{idVendor}=="0483", ATTRS{idProduct}=="df11", TAG+="uaccess"
    '';

    networking = {
      interfaces.enp0s25.useDHCP = true;
      bridges.br0.interfaces = [ "eth0" ];
      firewall = {
        checkReversePath = false;
        interfaces.eth0.allowedTCPPorts = [ 80 ];
      };
    };
  };
}
