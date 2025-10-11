{ pkgs, lib, ...}:
{
  home.packages = with pkgs; [
    (writeShellScriptBin "crb" (builtins.readFile ./crb.sh))
  ];
}
