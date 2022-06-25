{ lib, pkgs, ... }:
let
  nixos-hardware = builtins.fetchGit {
    name = "nixos-hardware";
    url = "https://github.com/NixOS/nixos-hardware.git";
  };
  nixpkgs-bcachefs = import (builtins.fetchTarball {
    name = "nixpkgs-for-bcachefs";
    url =
      "https://github.com/NixOS/nixpkgs/archive/efb6eb853e8b1546ad7760a31159bf1a8ea132b3.tar.gz";
    sha256 = "0kl5kx9g7ilfz05xa48gc8a6q5k73dzxksxdiqydvgb7sb84zbsb";
  }) { };
in {
  imports = [ "${nixos-hardware}/lenovo/thinkpad/x1-extreme" ];

  config = {
    a3 = {
      enable = true;
      hostname = "spirit";
      displayServer = "wayland";
      formFactor = "portable";
      esp = "/boot";
      buildSshKey = "id_ecdsa";
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

    # bcachefs needs 5.17.14. Builds on 5.17.15 and up appear to be broken. Remove this once nixpkgs
    # updates.
    boot.kernelPackages = lib.mkForce (pkgs.linuxPackagesFor
      (pkgs.linuxKernel.kernels.linux_testing_bcachefs.override {
        kernel = nixpkgs-bcachefs.linuxKernel.kernels.linux_5_17;
      }));
  };
}
