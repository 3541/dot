{ self, a3, ... }: {
  system = "x86_64-darwin";

  modules = [
    ({ lib, pkgs, ... }: {
      imports = [ a3.darwinModule ];

      config = {
        a3 = {
          enable = true;
          hostName = "sydmacx3bd";
          platform = "darwin";
          hardware.formFactor = "portable";

          home = {
            enable = true;
            user = "aobrien";
            directory = "/Users/aobrien";

            ui.fonts = {
              ui.size = 16.0;

              editor = {
                font = "Berkeley Mono";
                size = 13.0;
              };
            };
          };
        };

        users.users.aobrien = {
          home = "/Users/aobrien";
          shell = pkgs.bashInteractive;
        };

        system.keyboard = {
          enableKeyMapping = true;
          remapCapsLockToControl = true;
        };

        time.timeZone = lib.mkForce "America/Chicago";
      };
    })
  ];
}
