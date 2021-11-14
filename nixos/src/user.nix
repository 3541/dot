{ lib, config, ... }:
let cfg = config.a3;
in {
  config = {
    users.users.alex = {
      isNormalUser = true;
      extraGroups = [ "wheel" ]
        ++ lib.optional (cfg.displayServer != "none") "video"
        ++ lib.optional (cfg.role == "workstation") "libvirtd";
      openssh.authorizedKeys.keys = [
        "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBLTwFclui7JdzUVdxNI9pMEXs+d0FtDHfBf/rcfqjf5D3JdtEi6qehzdg/fZcG/ZR7d1dFY/pQem/8TnY4d/KNs= alex@spirit"
        "sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAIJwJvUR+hIuXjHYR2SPbNsRlbkWy4yCDF+QAQVidJWkMAAAABHNzaDo= alex@snafu"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINOa+tkiKGgQz0tLv5b9qswiSXfGdv4ThJpfxeIWOoLM alex@opportunity"
      ];
    };
  };
}
