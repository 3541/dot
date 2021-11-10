{ config, pkgs, ... }: {
  home.packages = [ pkgs.sakura ];

  home.file.sakuraConfig = {
    source = ./sakura.conf;
    target = ".config/sakura/sakura.conf";
  };
}
