{
  config,
  options,
  lib,
  ...
}:
let
  cfg = config.a3;
  enable = options ? networking;
in
{
  options.a3.hostname = lib.mkOption {
    type = lib.types.str;
  };

  config =
    { }
    // lib.optionalAttrs enable {
      networking.hostName = cfg.hostname;
    };
}
