{ lib, config, pkgs, ... }:
let cfg = config.a3;
in {
  config = lib.mkIf (cfg.role == "workstation") {
    services.printing.enable = true;
    services.printing.drivers = with pkgs; [ hll2390dw-cups epson-escpr2 ];

    sound.enable = true;
    hardware.pulseaudio = {
      enable = true;
      support32Bit = true;
      extraConfig = ''
        load-module module-echo-cancel use_master_format=1 aec_method=webrtc aec_args="analog_gain_control=0\ digital_gain_control=1" source_name=echoCancel_source sink_name=echoCancel_sink
        set-default-source echoCancel_source
      '';
    };

    virtualisation.libvirtd = {
      enable = true;
      qemu = {
        swtpm.enable = true;
        # OVMFFull is broken at the moment. https://github.com/NixOS/nixpkgs/issues/164064
        ovmf.package = pkgs.OVMF.override {
          secureBoot = true;
          tpmSupport = true;
        };
      };
    };
    virtualisation.docker = {
      enable = true;
      extraOptions = "--ipv6 --fixed-cidr-v6 fd00::/80";
    };

    programs.gnupg.agent.enable = true;
    services.pcscd.enable = true;
    services.vnstat.enable = true;

    programs.dconf.enable = true;

    services.usbmuxd.enable = true;

    # Temporarily disabled. Breaks iptables command (and consequently libvirtd) in NixOS 22.05.
    # services.opensnitch.enable = cfg.role == "workstation";
    environment.systemPackages = [ pkgs.opensnitch-ui ];

    programs.ssh.startAgent = true;
  };
}