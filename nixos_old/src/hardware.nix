{ lib, config, ... }:
let cfg = config.a3;
in {
  config = {
    hardware.cpu.intel.updateMicrocode = cfg.cpu == "intel";
    hardware.cpu.amd.updateMicrocode = cfg.cpu == "amd";
    services.fwupd.enable = true;

    services.thermald.enable = cfg.formFactor == "portable" && cfg.cpu == "intel";
    powerManagement.powertop.enable = cfg.formFactor == "portable";
  };
}
