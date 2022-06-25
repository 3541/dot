{ config, lib, ... }:
let cfg = config.a3;
in {
  options.a3.build.sshKey = lib.mkOption {
    type = lib.types.str;
    default = "id_ed25519";
  };

  config.nix = lib.mkIf cfg.enable {
    buildMachines = builtins.filter (m: m.hostName != cfg.hostName)
      (builtins.map (s:
        s // {
          sshUser = "alex";
          sshKey = "/home/alex/.ssh/${cfg.build.sshKey}";
        }) [
          {
            hostName = "opportunity";
            system = "x86_64-linux";
            maxJobs = 12;
            speedFactor = 2;
            supportedFeatures = [ "kvm" "big-parallel" ];
          }
          {
            hostName = "spirit";
            system = "x86_64-linux";
            maxJobs = 8;
            speedFactor = 1;
            supportedFeatures = [ "kvm" ];
          }
        ]);

    distributedBuilds = true;
  };
}
