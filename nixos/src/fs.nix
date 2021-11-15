{ lib, config, ... }:
let cfg = config.a3;
in {
  config = {
    boot.tmpOnTmpfs = !cfg.smallMemory;
    boot.cleanTmpDir = cfg.smallMemory;
    fileSystems."/mnt/net_share" = lib.mkIf (cfg.role == "workstation") {
      device = "//sagittarius/share";
      fsType = "cifs";
      options = [
        "x-systemd.automount"
        "noauto"
        "x-systemd.idle-timeout=60"
        "x-systemd.device-timeout=5s"
        "x-systemd.mount-timeout=5s"
        "credentials=/etc/nixos/smbcredentials"
        "uid=alex"
        "gid=users"
      ];
    };
  };
}
