{ lib, ... }: {
  a3 = {
    hostname = "medina";
    gui.enable = false;
    autoupdate = true;

    system = {
      os = "linux";
      role = "server";
    };
  };

  time.timeZone = "Asia/Hong_Kong";

  services.openssh.enable = lib.mkForce false;
}
