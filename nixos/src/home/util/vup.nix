{ cfg, lib, pkgs, ... }: {
  config.home = lib.mkIf (cfg.enable && cfg.home.enable && cfg.role == "workstation") {
    packages = [ (pkgs.writeShellScriptBin "vup" (builtins.readFile ./vup.sh)) ];
  };
}
