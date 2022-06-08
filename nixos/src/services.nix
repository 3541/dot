{ lib, config, ... }:
let cfg = config.a3;
in {
  config = {
    services.openssh = lib.mkIf (cfg.formFactor != "portable") {
      enable = true;
      permitRootLogin = "no";
      passwordAuthentication = false;
    };

    services.tailscale.enable = true;
  };
}
