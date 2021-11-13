{ lib, config, pkgs, ... }:
let cfg = config.a3;
in {
  config = lib.mkIf (cfg.role == "workstation") {
    home.packages = [ pkgs.gdb ];
    home.file.gdbinit = {
      text = "set disassembly-flavor intel";
      target = ".gdbinit";
    };
  };
}
