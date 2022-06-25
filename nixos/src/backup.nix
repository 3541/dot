{ config, lib, ... }:
let cfg = config.a3;
in {
  options.a3.backup = {
    enable = lib.mkEnableOption "Enable backup via borgbackup.";

    repo = lib.mkOption {
      type = lib.types.str;
      default = cfg.hostName;
    };
  };

  config.services.borgbackup.jobs.backup =
    lib.mkIf (cfg.enable && cfg.backup.enable) {
      repo = "ssh://borg@sagittarius/share/backup-${cfg.backup.repo}";
      paths = [ "/home/alex" "/etc/nixos" ];
      environment.BORG_RSH = "ssh -i /root/.ssh/id_ed25519";
      startAt = "daily";
      doInit = false;

      prune.keep = {
        daily = 10;
        weekly = 7;
        monthly = 4;
        yearly = 12;
      };

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

      encryption = {
        passCommand = "cat /etc/nixos/borg_passphrase";
        mode = "repokey-blake2";
      };
    };
}
