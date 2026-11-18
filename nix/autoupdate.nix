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
          path = with pkgs; [ lix systemd git nixos-rebuild ];
          script = ''
            set -euo pipefail

            cd "/home/${cfg.user.name}/dot/flakes/nixos"
            nix flake update
            nixos-rebuild switch --flake .
            systemctl reboot
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
