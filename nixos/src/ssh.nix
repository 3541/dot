{ config, lib, pkgs, ... }:
let cfg = config.a3;
in {
  config = lib.mkIf cfg.enable {
    services.openssh = {
      enable = true;

      settings = {
        PermitRootLogin = "no";
        PasswordAuthentication = false;
      };
    };

    services.sshguard.enable = true;
  };
}
