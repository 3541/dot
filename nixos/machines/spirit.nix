let nixos-hardware = builtins.fetchGit { url = "https://github.com/NixOS/nixos-hardware.git"; };
in {
  imports = [ "${nixos-hardware}/lenovo/thinkpad/x1-extreme" ];

  config = {
    a3 = {
      enable = true;
      hostname = "spirit";
      displayServer = "wayland";
      formFactor = "portable";
      esp = "/boot";
    };

    swapDevices = [{ device = "/swapfile"; }];

    networking.interfaces.enp0s31f6.useDHCP = true;
    networking.interfaces.wlp0s20f3.useDHCP = true;

    hardware.video.hidpi.enable = true;
    services.xserver.dpi = 144;
  };
}
