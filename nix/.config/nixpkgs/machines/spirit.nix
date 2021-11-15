{
  a3 = {
    enable = true;
    platform = "nixos";
    displayServer = "wayland";
    potato = true;
    fontSize = 16.0;
  };

  wayland.windowManager.sway.config = {
    output = {
      "eDP-1" = {
        resolution = "3840x2160";
        scale = "1.0";
      };
      "HDMI-A-1" = {
        resolution = "1280x720";
        pos = "0 0";
        scale = "0.5";
      };
    };

    input = {
      "type:touchpad".events = "disabled";
      "type:touch".events = "disabled";
      "type:tablet tool".events = "disabled";
      "type:pointer".pointer_accel = "1.0";
    };
  };
}
