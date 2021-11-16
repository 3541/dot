{ lib, config, ... }:
let cfg = config.a3;
in {
  config = {
    networking.hostName = cfg.hostname;
    networking.networkmanager.enable = cfg.formFactor == "portable";
  };
}
