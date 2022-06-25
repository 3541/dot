{ config, lib, ... }:
let cfg = config.a3;
in {
  config = lib.mkIf cfg.enable {
    services.tailscale.enable = true;

    networking = {
      hostName = cfg.hostName;
      networkmanager.enable = cfg.hardware.formFactor == "portable";
      firewall.checkReversePath = "loose";
      nameservers = [ "192.168.1.1" "1.1.1.1" "100.100.100.100" ];
      search = [ "3541.github.beta.tailscale.net" ];
    };
  };
}
