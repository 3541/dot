{ lib, config, ... }:
let cfg = config.a3;
in {
  config = {
    networking.hostName = cfg.hostname;
    networking.wireless.enable = cfg.formFactor == "portable";
  };
}
