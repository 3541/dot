{ lib, ... }:
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
      # Turn this off when 21.11 lands and tmpOnTmpfsSize can be configured.
      smallMemory = true;
    };

    swapDevices = [{ device = "/swapfile"; }];

    networking.interfaces.enp0s31f6.useDHCP = true;
    networking.interfaces.wlp0s20f3.useDHCP = true;

    hardware.video.hidpi.enable = true;
    environment.variables.GDK_DPI_SCALE = "1.5";
    services.xserver = {
      videoDrivers = [ "modesetting" ];
      dpi = 144;
    };
    # Once nVidia 490.44 lands, it should be possible to switch to PRIME Sync mode, instead (and
    # hopefully get external displays working).
    hardware.bumblebee = {
      enable = true;
      connectDisplay = true;
    };

    # Something is broken with intel-gmmlib and i686 support.
    programs.steam.enable = lib.mkForce false;
    hardware.opengl.driSupport32Bit = lib.mkForce false;
  };
}
