{ a3, ... }: {
  system = "x86_64-linux";
  modules = [
    ({ lib, pkgs, modulesPath, ... }: {
      imports = [
        # (modulesPath + "/profiles/minimal.nix")
        a3.nixosModule
        ./HOSTNAME-hardware.nix
      ];

      config = {
        a3 = {
          enable = true;
          hostName = "HOSTNAME";
          encryptRoot = false;
          role = "server";
          minimal = true;
          fs.tmpOnTmpfs = false;
          home.enable = true;

          boot = {
            loader = "grub";
            method = "bios";
            device = DISK;
          };
        };
      };
    })
  ];
}
