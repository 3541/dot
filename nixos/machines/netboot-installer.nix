{ a3, ... }: {
  system = "x86_64-linux";
  modules = [
    ({ modulesPath, lib, pkgs, ... }: {
      imports = [
        a3.nixosModule
        (modulesPath + "/installer/netboot/netboot-minimal.nix")
      ];

      config = {
        a3 = {
          enable = true;
          hostName = "nixos-installer";
          boot.loader = "other";
          boot.method = "other";
          role = "server";
          minimal = true;
        };

        services.openssh.permitRootLogin = lib.mkForce "no";

        environment.systemPackages = [
          pkgs.git
          (pkgs.writeShellScriptBin "install" (builtins.readFile ./install.sh))
        ];
      };
    })
  ];
}
