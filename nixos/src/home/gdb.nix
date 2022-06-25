{ cfg, lib, pkgs, ... }: {
  config.home =
    lib.mkIf (cfg.enable && cfg.home.enable && cfg.role == "workstation") {
      packages = [ pkgs.gdb ];
      file.gdbinit = {
        text = "set disassembly-flavor intel";
        target = ".gdbinit";
      };
    };
}
