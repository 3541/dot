{ config, lib, ... }:
let cfg = config.a3;
in {
  options.a3.fs.tmpOnTmpfs = lib.mkOption {
    type = lib.types.bool;
    default = true;
  };

  config = lib.mkIf cfg.enable {
    boot = {
      tmpOnTmpfs = cfg.fs.tmpOnTmpfs;
      tmpOnTmpfsSize = lib.optionalString cfg.fs.tmpOnTmpfs "95%";
      cleanTmpDir = !cfg.fs.tmpOnTmpfs;
    };

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
