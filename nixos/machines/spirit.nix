{ lib, ... }:
let
  nixos-hardware =
    builtins.fetchGit { url = "https://github.com/NixOS/nixos-hardware.git"; };
in {
  imports = [ "${nixos-hardware}/lenovo/thinkpad/x1-extreme" ];

  config = {
    a3 = {
      enable = true;
      hostname = "spirit";
      displayServer = "wayland";
      formFactor = "portable";
      esp = "/boot";
    };

    networking.interfaces.enp0s31f6.useDHCP = true;
    networking.interfaces.wlp0s20f3.useDHCP = true;

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
      repo = "ssh://borg@sagittarius/share/backup-spirit";
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

    hardware.video.hidpi.enable = true;
    environment.variables.GDK_DPI_SCALE = "1.5";
    services.xserver = {
      videoDrivers = [ "modesetting" "nouveau" ];
      dpi = 96;
    };

    programs.steam.enable = true;

    services.undervolt = {
      enable = true;
      coreOffset = -160;
      uncoreOffset = -160;
    };
  };
}
