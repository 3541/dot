{ config, lib, pkgs, ... }:
let cfg = config.a3;
in {
  config = lib.mkIf (cfg.enable && cfg.hardware.formFactor != "portable") {
    services.openssh = {
      enable = true;
      permitRootLogin = "no";
      passwordAuthentication = false;
    };

    services.sshguard.enable = true;
  };
}
