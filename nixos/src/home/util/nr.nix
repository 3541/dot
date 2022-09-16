{ cfg, lib, pkgs, ... }: {
  config.home = lib.mkIf (cfg.enable && cfg.home.enable) {
    packages = [ (pkgs.writeShellScriptBin "nr" (builtins.readFile ./nr.sh)) ];
  };
}
