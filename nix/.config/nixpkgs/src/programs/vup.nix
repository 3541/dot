{ lib, config, pkgs, ... }:
let cfg = config.a3;
in {
  config = lib.mkIf (cfg.role == "workstation" && cfg.platform != "macOS") {
    home.packages = with pkgs;
      [ (pkgs.writeShellScriptBin "vup" (builtins.readFile ./vup.sh)) ];
  };
}
