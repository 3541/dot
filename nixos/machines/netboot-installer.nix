{ ... }: {
  system = "x86_64-linux";
  modules = [
    ({ modulesPath, ... }: {
      imports = [ (modulesPath + "/installer/netboot/netboot-minimal.nix") ];

      config.system.stateVersion = "22.05";
    })
  ];
}
