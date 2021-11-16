{ lib, config, pkgs, ... }:
let
  cfg = config.a3;
in {
  config = lib.mkIf (cfg.role == "workstation") {
    home.packages = [ pkgs.cmus ];
    home.file.cmusConfig = {
      source = ./cmus_config;
      target = ".config/cmus/autosave";
    };
  };
}
