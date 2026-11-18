{
  config,
  options,
  lib,
  ...
}:
let
  cfg = config.a3;
  enable = options ? services.openssh;
in
{
  config =
    { }
    // lib.optionalAttrs enable {
      services.openssh = lib.mkIf (cfg.orchestrator == "nixos") {
        enable = true;

        settings = {
          PermitRootLogin = "no";
          PasswordAuthentication = false;
        };
      };
    };
}
