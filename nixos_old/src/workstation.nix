{ lib, config, ... }:
let cfg = config.a3;
in {
  config = lib.mkIf (cfg.role == "workstation") {
    security.pam.loginLimits = [{
      domain = "*";
      type = "hard";
      item = "memlock";
      value = "1024";
    }];
  };
}
