{ pkgs, ... }: {
  home.packages = with pkgs;
    [
      (writeShellScriptBin "me3t" ''
        WINEPREFIX=/mass/games/me3t/wine ${wineWowPackages.staging}/bin/wine64 /mass/games/me3t/ME3TweaksModManager.exe
      '')
    ];
}
