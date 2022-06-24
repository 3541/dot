{
  a3 = {
    enable = true;
    platform = "nixos";
    displayServer = "wayland";
    potato = true;
    fontSize = 16.0;
    formFactor = "portable";
    editorFont = "Berkeley Mono";
  };

  wayland.windowManager.sway.config = {
    output = {
      "eDP-1" = {
        resolution = "3840x2160";
        scale = "1.0";
      };
      "HDMI-A-1".resolution = "1920x1080";
    };

    input = {
      "type:touchpad".events = "disabled";
      "type:touch".events = "disabled";
      "type:tablet tool".events = "disabled";
      "type:pointer" = {
        pointer_accel = "1.0";
        accel_profile = "adaptive";
      };
      "type:keyboard".xkb_options = "ctrl:nocaps";
    };
  };
}
