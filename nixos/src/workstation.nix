{ config, lib, pkgs, ... }:
let cfg = config.a3;
in {
  config = lib.mkIf (cfg.enable && cfg.role == "workstation") {
    sound.enable = true;

    hardware.pulseaudio = {
      enable = true;
      support32Bit = true;
      extraConfig = ''
        load-module module-echo-cancel use_master_format=1 aec_method=webrtc aec_args="analog_gain_control=0\ digital_gain_control=1" source_name=echoCancel_source sink_name=echoCancel_sink
        set-default-source echoCancel_source
      '';
    };

    security.pam.loginLimits = [{
      domain = "*";
      type = "hard";
      item = "memlock";
      value = "1024";
    }];

    services = {
      pcscd.enable = true;
      vnstat.enable = true;
      usbmuxd.enable = true;

      printing = {
        enable = true;
        drivers = with pkgs; [ hll2390dw-cups epson-escpr2 ];
      };
    };

    environment.systemPackages = with pkgs; [ opensnitch-ui aerc pandoc ];

    programs = {
      gnupg.agent.enable = true;
      dconf.enable = true;
      ssh.startAgent = true;
    };

    virtualisation = {
      libvirtd = {
        enable = true;

        qemu = {
          swtpm.enable = true;
          # OVMFFull is broken at the moment. https://github.com/NixOS/nixpkgs/issues/164064
          ovmf.packages = [
            (pkgs.OVMF.override {
              secureBoot = true;
              tpmSupport = true;
            }).fd
          ];
        };
      };

      podman = {
        enable = true;
        dockerCompat = true;
      };
    };
  };
}
