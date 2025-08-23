{ pkgs, lib, ...}:
{
  home.packages = with pkgs; [
    (writeShellScriptBin "up" (builtins.readFile ./up.sh))
  ];
}
