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

        environment.systemPackages = [
          pkgs.git
          (pkgs.writeShellScriptBin "a3-install" (builtins.readFile ./install.sh))
        ];

        services.openssh.permitRootLogin = lib.mkForce "no";
        users.motd = "Run a3-install to begin installation.";
      };
    })
  ];
}
