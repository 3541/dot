{ pkgs, ... }: {
  a3 = {
    hostname = "sagittarius";

    boot = {
      loader = "grub";
      device = "/dev/sda";
    };

    system = {
      os = "linux";
      role = "server";
    };
  };

  time.timeZone = "Asia/Hong_Kong";

  environment.systemPackages = [ pkgs.borgbackup ];

  services.samba = {
    enable = true;
    openFirewall = true;

    settings = {
      global = {
        workgroup = "AOBNET";
        "server string" = "sagittarius";
        "netbios name" = "sagittarius";
        security = "user";
        "hosts allow" = "192.168.0. 127.0.0.1 localhost";
        "hosts deny" = "0.0.0.0/0";
        "guest account" = "nobody";
        "map to guest" = "bad user";
      };

      share = {
        path = "/mass/share";
        "valid users" = "alex";
        browseable = "yes";
        "read only" = "no";
        "guest ok" = "no";
        "create mask" = "0644";
        "directory mask" = "0755";
        "force user" = "alex";
        "force group" = "users";
        "vfs objects" = "fruit";
      };

      tm = {
        path = "/mass/tm";
        "valid users" = "alex";
        public = "no";
        writeable = "yes";
        "force user" = "alex";
        "force group" = "users";
        "fruit:aapl" = "yes";
        "fruit:time machine" = "yes";
        "vfs objects" = "catia fruit streams_xattr";
      };
    };
  };
}
