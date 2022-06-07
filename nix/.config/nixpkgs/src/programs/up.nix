{ pkgs, ... }: {
  home.packages = with pkgs;
    [ (writeShellScriptBin "up" (builtins.readFile ./up.sh)) ];
}
