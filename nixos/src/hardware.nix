{ config, lib, ... }:
let cfg = config.a3;
in {
  options.a3.hardware = {
    formFactor = lib.mkOption {
      type = lib.types.enum [ "fixed" "portable" ];
      default = "fixed";
    };

    cpu = lib.mkOption {
      type = lib.types.enum [ "intel" "amd" "arm" ];
      default = "intel";
    };
  };

  config = lib.mkIf cfg.enable {
    powerManagement.powertop.enable = cfg.hardware.formFactor == "portable";

    services = {
      smartd.enable = true;
      fwupd.enable = cfg.hardware.cpu == "intel" || cfg.hardware.cpu == "amd";
      thermald.enable = cfg.hardware.formFactor == "portable"
        && cfg.hardware.cpu == "intel";
    };

    hardware.cpu = {
      intel.updateMicrocode = cfg.hardware.cpu == "intel";
      amd.updateMicrocode = cfg.hardware.cpu == "amd";
    };
  };
}
