{ lib, ... }:
{
  options.a3.home = {
    shell = {
      nuExtra = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
      };
    };
  };
}
