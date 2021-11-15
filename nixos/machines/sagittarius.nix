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
      securityType = "user";
      extraConfig = ''
        server string = sagittarius
        netbios name = sagittarius
        security = user
        log level = 4
        log file = /var/log/samba/samba.log
        hosts allow = 192.168.1.0/24
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
    };

    # Open ports in the firewall.
    # networking.firewall.allowedTCPPorts = [ ... ];
    # networking.firewall.allowedUDPPorts = [ ... ];
    # Or disable the firewall altogether.
    # networking.firewall.enable = false;
    networking.firewall.allowPing = true;
    networking.firewall.allowedTCPPorts = [
      # samba
      445
      139
    ];
    networking.firewall.allowedUDPPorts = [
      # samba
      137
      138
    ];
  };
}
