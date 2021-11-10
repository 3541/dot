{ config, pkgs, ... }: {
  home.packages = [ pkgs.gdb ];
  home.file.gdbinit = {
    text = "set disassembly-flavor intel";
    target = ".gdbinit";
  };
}
