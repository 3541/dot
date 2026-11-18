{
  config,
  options,
  lib,
  ...
}:
let
  cfg = config.a3;
  enable = options ? boot;
in
{
  options.a3.boot = {
    loader = lib.mkOption {
      type = lib.types.enum [ "grub" "none" ];
      default = "none";
    };

    device = lib.mkOption {
      type = lib.types.str;
    };
  };

  config =
    { }
    // lib.optionalAttrs enable {
      boot.loader.grub = lib.mkIf (cfg.orchestrator == "nixos" && cfg.boot.loader == "grub") {
        enable = true;
        device = cfg.boot.device;
      };
    };
}
