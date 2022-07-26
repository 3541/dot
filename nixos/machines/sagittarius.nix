{ a3, ... }: {
  system = "x86_64-linux";
  modules = [
    ({ lib, pkgs, modulesPath, ... }: {
      imports = [
        (modulesPath + "/profiles/minimal.nix")
        a3.nixosModule
        ./sagittarius-hardware.nix
      ];

      config = {
        a3 = {
          enable = true;
          hostName = "sagittarius";
          encryptRoot = false;
          role = "server";
          minimal = true;
          fs.tmpOnTmpfs = false;
          home.enable = true;
          minimal = true;

          boot = {
            loader = "grub";
            method = "bios";
            device = "/dev/disk/by-id/usb-SMI_USB_DISK-0:0";
          };
        };

        services = {
          samba = {
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

          borgbackup.repos = {
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
      };
    })
  ];
}
