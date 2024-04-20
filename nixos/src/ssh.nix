{ config, lib, pkgs, ... }:
let cfg = config.a3;
in {
  config = lib.mkIf cfg.enable {
    programs.mosh.enable = true;

    services = {
      sshguard.enable = true;

      openssh = {
        enable = true;

        settings = {
          PermitRootLogin = "no";
          PasswordAuthentication = false;
        };
      };
    };
  };
}
