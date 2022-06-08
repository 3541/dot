{ lib, config, ... }:
let cfg = config.a3;
in {
  config = {
    services.tailscale.enable = true;
    networking = {
      hostName = cfg.hostname;
      networkmanager.enable = cfg.formFactor == "portable";

      firewall.checkReversePath = "loose";
      nameservers = [ "100.100.100.100" " 1.1.1.1" ];
      search = [ "3541.github.beta.tailscale.net" ];
    };
  };
}
