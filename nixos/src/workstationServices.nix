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

    virtualisation.libvirtd.enable = true;

    programs.gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
    services.pcscd.enable = true;
    services.vnstat.enable = true;

    programs.dconf.enable = true;

    services.usbmuxd.enable = true;

    services.opensnitch.enable = cfg.role == "workstation";
    environment.systemPackages = [ pkgs.opensnitch-ui ];
  };
}
