{ lib, config, ... }:
let cfg = config.a3;
in {
  config = {
    security.pam.loginLimits = lib.mkIf (cfg.role == "workstation") [{
      domain = "*";
      type = "hard";
      item = "memlock";
      value = "1024";
    }];
  };
}
