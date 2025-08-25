{ lib, ... }:
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
}
