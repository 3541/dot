{
  config,
  options,
  lib,
  ...
}:
let
  cfg = config.a3;
  enable = options ? users;
in
{
  options.a3.user = {
    name = lib.mkOption {
      type = lib.types.str;
      default = "alex";
    };

    home = lib.mkOption {
      type = lib.types.path;
      default = /home/alex;
    };

    fullName = lib.mkOption {
      type = lib.types.str;
      default = "Alex O'Brien";
    };

    email = lib.mkOption {
      type = lib.types.str;
      default = "3541@3541.website";
    };
  };


  config =
    { }
    // lib.optionalAttrs enable {
      users.users.${cfg.user.name} = assert cfg.orchestrator == "nixos"; {
        isNormalUser = true;
        extraGroups = lib.optional (cfg.system.os == "linux") "wheel";

        openssh.authorizedKeys.keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINCRv8rbBQ/hjEtUXwKAPEFAjkAmnhIBoR+9bGlBSyBu alex@opportunity"
        ];
      };
    };
}
