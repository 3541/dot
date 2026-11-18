{
  config,
  pkgs,
  options,
  lib,
  ...
}:
let
  cfg = config.a3;
  enable = options ? systemd.services;
in
{
  options.a3.autoupdate = lib.mkEnableOption "auto-update";

  config =
    { }
    // lib.optionalAttrs enable {
      systemd = lib.mkIf cfg.autoupdate {
        services.autoupdate = {
          script = ''
            set -euo pipefail

            cd "${cfg.user.home}/dot/flakes/nixos"
            nix flake update
            ${pkgs.nh} os switch .
            ${pkgs.systemd}/bin/systemctl reboot
          '';

          serviceConfig = {
            Type = "oneshot";
            User = "root";
            RemainAfterExit = true;
          };
        };

        timers.autoupdate = {
          wantedBy = [ "timers.target" ];

          timerConfig = {
            OnBootSec = "24h";
            OnUnitActiveSec = "24h";
            Unit = "autoupdate.service";
          };
        };
      };
    };
}
