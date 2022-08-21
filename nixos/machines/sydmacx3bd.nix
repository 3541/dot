{ self, a3, ... }: {
  system = "x86_64-darwin";

  modules = [
    ({ lib, pkgs, ... }: {
      imports = [ a3.nixosModule ];

      config = {
        a3 = {
          enable = true;
          hostname = "sydmacx3bd";
          build.distributed = false;
          hardware.formFactor = "portable";

          home = {
            enable = true;

            ui.fonts = {
              ui.size = 16.0;

              editor = {
                font = "Berkeley Mono";
                size = 15.0;
              };
            };
          };
        };
      };
    })
  ];
}
