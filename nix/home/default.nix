{ state-version, home-manager }:
{
  config,
  lib,
  pkgs,
  package-inputs,
  ...
}:
let
  cfg = config.a3;
in
{
  imports = [ home-manager ];

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
  options.a3.shell = {
    nuExtra = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
    };
  };

  config = {
    users.users.${cfg.user.name} = {
      name = cfg.user.name;
      home = cfg.user.home;
    };

    home-manager = {
      useGlobalPkgs = true;
      useUserPackages = true;

      extraSpecialArgs = {
        cfg = config.a3;
        package-inputs = package-inputs;
      };

      users.${cfg.user.name} = {
        programs.home-manager.enable = true;

        home = {
          stateVersion = state-version;
          username = cfg.user.name;
          homeDirectory = cfg.user.home;
        };

        imports = [
          ./alacritty.nix
          ./helix.nix
          ./packages.nix
          ./shell.nix
          ./util
          ./vc.nix
        ];
      };
    };
  };
}
