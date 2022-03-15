{ lib, config, ... }:
let cfg = config.a3;
in {
  config.networking = {
    hostName = cfg.hostname;
    networkmanager.enable = cfg.formFactor == "portable";
  };
}
