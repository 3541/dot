{ config, lib, pkgs, ... }:
let cfg = config.a3;
in {
  options.a3.user = {
    authorizedKeys = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [
        "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBLTwFclui7JdzUVdxNI9pMEXs+d0FtDHfBf/rcfqjf5D3JdtEi6qehzdg/fZcG/ZR7d1dFY/pQem/8TnY4d/KNs= alex@spirit"
        "sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAIJwJvUR+hIuXjHYR2SPbNsRlbkWy4yCDF+QAQVidJWkMAAAABHNzaDo= alex@snafu"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINOa+tkiKGgQz0tLv5b9qswiSXfGdv4ThJpfxeIWOoLM alex@opportunity"
        "ecdsa-sha2-nistp384 AAAAE2VjZHNhLXNoYTItbmlzdHAzODQAAAAIbmlzdHAzODQAAABhBNVriBufeoOaxGnYbokJm1xE/LI2NkjLjx6ikQZmpkxgT3iMRwcfv5ebjyx0h39z/8M4llXnrFqEMjoO2Ltj3kXjPEXcNlQcQ23SfBnawkgnhfosGeMUbPIwFQxjnAeqmQ== Generated By Termius"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDhFMPPiVu23qzo2gWjm8RCsaOC/xn8559oVdyAuryLy alex@viking-1.my.domain"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFSbGBJbe77zLRSiY2+2j1FGD28KtxPLZx7zrfyuNz/i alex@sagittarius"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGsYGu42ZEcbU/0qLskvmGn3MS5qr3+OqAD5WVG9KROL alex@curiosity"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIO7qC2UssyXWyYdB9jomph1zoNEOjjHBsQj1ccG8AHjl alex@curiosity.local"
      ];
    };

    groups = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
    };
  };

  config.users.users.alex = lib.mkIf cfg.enable {
    isNormalUser = true;
    initialPassword = "changeme";
    shell = pkgs.nushell;

    extraGroups = [ "wheel" ] ++ lib.optional cfg.display.enable "video"
      ++ lib.optional (cfg.role == "workstation") "libvirtd"
      ++ lib.optional (cfg.role == "workstation") "docker"
      ++ lib.optional (cfg.role == "workstation") "scanner"
      ++ lib.optional (cfg.role == "workstation") "lp"
      ++ lib.optional (cfg.hardware.formFactor == "portable") "networkmanager"
      ++ cfg.user.groups;

    openssh.authorizedKeys.keys = cfg.user.authorizedKeys;
  };
}
