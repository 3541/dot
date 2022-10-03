{ config, lib, ... }:
let cfg = config.a3;
in {
  config = lib.mkIf cfg.enable {
    time.timeZone = "Australia/Sydney";

    i18n.defaultLocale = "en_US.UTF-8";
    console = {
      font = "Lat2-Terminus16";
      keyMap = "us";
    };
  };
}
