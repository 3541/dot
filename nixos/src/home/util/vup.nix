{ cfg, lib, pkgs, ... }: {
  config.home = lib.mkIf (cfg.enable && cfg.home.enable) {
    packages = [ (pkgs.writeShellScriptBin "vup" (builtins.readFile ./vup.sh)) ];
  };
}
