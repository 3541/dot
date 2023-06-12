{ config, lib, pkgs, firefox, ... }:
let cfg = config.a3;
in {
  config = lib.mkIf cfg.enable {
    services.tailscale.enable = true;

    networking = {
      hostName = cfg.hostName;
      networkmanager.enable = cfg.hardware.formFactor == "portable";
      firewall.checkReversePath = "loose";
      nameservers = [ "192.168.0.1" "100.100.100.100" ];
      search = [ "3541.github.beta.tailscale.net" ];
    };

    services.opensnitch = lib.mkIf (cfg.role == "workstation") {
      enable = true;
      # nftables breaks current iptables command, and system rules are not supported.
      settings.Firewall = "iptables";

      rules = let
        allowBinary = name: path: {
          inherit name;
          enabled = true;
          action = "allow";
          duration = "always";
          operator = {
            type = "simple";
            operand = "process.path";
            data = "${lib.getBin pkgs.${name}}/${path}";
          };
        };

        allowPackage = name: allowBinary name "bin/${name}";
      in {
        thunderbird = allowPackage "thunderbird";
        nsncd = allowPackage "nsncd";
        nix = allowPackage "nix";
        systemd-timesyncd =
          allowBinary "systemd" "lib/systemd/systemd-timesyncd";
        syncthing = allowPackage "syncthing";
        tailscale = allowBinary "tailscale" "bin/.tailscaled-wrapped";
        ssh = allowBinary "openssh" "bin/ssh";

        firefox = {
          name = "firefox";
          enabled = true;
          action = "allow";
          duration = "always";
          operator = {
            type = "simple";
            operand = "process.path";
            data = "${
                lib.getBin firefox.packages.${cfg.system}.firefox
              }/lib/firefox/firefox";
          };
        };
      };
    };

    environment.etc = lib.mkIf (cfg.role == "workstation") {
      "opensnitchd/system-fw.json".source =
        ((pkgs.formats.json { }).generate "system-fw.json" {
          SystemRules = [
            {
              Rule = {
                Description = "Allow cifs";
                Table = "mangle";
                Chain = "OUTPUT";
                Parameters = "-p tcp --dport 445";
                Target = "ACCEPT";
                TargetParameters = "";
              };
            }
            {
              Rule = {
                Description = "Allow ICMP";
                Table = "mangle";
                Chain = "OUTPUT";
                Parameters = "-p icmp --icmp-type echo-request";
                Target = "ACCEPT";
                TargetParameters = "";
              };
            }
            {
              Rule = {
                Description = "Allow ICMPv6";
                Table = "mangle";
                Chain = "OUTPUT";
                Parameters = "-p ipv6-icmp --icmp-type echo-request";
                Target = "ACCEPT";
                TargetParameters = "";
              };
            }
          ];
        });
    };
  };
}
