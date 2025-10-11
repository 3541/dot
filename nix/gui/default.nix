{ lib, ... }:
{
  imports = [
    ./aerospace.nix
    ./sway.nix
  ];

  options.a3.gui = {
    enable = lib.mkEnableOption "gui" // {
      default = true;
    };

    font = {
      text = {
        size = lib.mkOption {
          type = lib.types.int;
          default = 13;
        };

        family = lib.mkOption {
          type = lib.types.str;
          default = "Berkeley Mono";
        };
      };

      ui = {
        size = lib.mkOption {
          type = lib.types.int;
          default = 13;
        };

        family = lib.mkOption {
          type = lib.types.str;
          default = "Berkeley Mono";
        };
      };
    };
  };
}
