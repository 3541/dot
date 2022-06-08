{
  imports = [ <nixpkgs/nixos/modules/profiles/minimal.nix> ];

  config = {
    a3 = {
      enable = true;
      boot = "bios";
      bootDevice = "/dev/disk/by-id/usb-SMI_USB_DISK-0:0";
      luks = false;
      grub = true;
      smallMemory = true;
      hostname = "sagittarius";
      role = "server";
    };

    services.samba = {
      enable = true;
      openFirewall = true;
      extraConfig = ''
        server string = sagittarius
        netbios name = sagittarius
        hosts allow = 192.168.1.0/24 100.64.0.0/10
        smb encrypt = required
      '';

      shares = {
        share = {
          path = "/share";
          browseable = "yes";
          writeable = "yes";
          "guest ok" = "no";
          "valid users" = "alex";
          "force user" = "alex";
          "force group" = "users";
        };
      };
    };

    services.borgbackup.repos = {
      opportunity = {
        authorizedKeys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIFqfY1FFHHJPX/VzNEqsAbLDBYkwyXuJqPwYm9llqmc root@opportunity"
        ];
        path = "/share/backup-marx";
      };
      spirit = {
        authorizedKeys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIChbiN2rLQEFvc/hmjWCWBU4nBvl9NOw3X320E5my3es root@spirit"
        ];
        path = "/share/backup-spirit";
      };
    };
  };
}
