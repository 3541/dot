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
  boot.loader.grub.configurationLimit = 1;

  networking.firewall.allowedTCPPorts = [
    80
    443
  ];

  security.acme = {
    acceptTerms = true;
    defaults.email = "3541@3541.website";
  };

  services = {
    openssh.enable = lib.mkForce false;

    nginx = {
      enable = true;
      recommendedProxySettings = true;
      recommendedOptimisation = true;
      recommendedTlsSettings = true;
      recommendedGzipSettings = true;

      virtualHosts."testing.3541.website" = {
        enableACME = true;
        forceSSL = true;
        http2 = false;

        locations."/" = {
          return = "200 '<h1>It works.</h1>'";
          extraConfig = "default_type text/html;";
        };
      };
    };
  };
}
