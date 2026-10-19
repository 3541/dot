{
  cfg,
  pkgs,
  lib,
  ...
}:
{
  home.packages =
    with pkgs;
    [
      (writeShellScriptBin "crb" (builtins.readFile ./crb.sh))
    ]
    ++ lib.optionals (cfg.system.role == "workstation" && cfg.system.os == "linux") [
      (writeShellScriptBin "vrun" (builtins.readFile ./vrun.sh))
      (writeShellScriptBin "vup" (builtins.readFile ./vup.sh))
    ];
}
