{ cfg, lib, pkgs, ... }: {
  config.home = lib.mkIf (cfg.enable && cfg.home.enable && cfg.role == "workstation") {
    packages =
      [ (pkgs.writeShellScriptBin "vrun" (builtins.readFile ./vrun.sh)) ];
  };
}
