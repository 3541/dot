{ config, ... }:
let cfg = config.a3;
in {
  config.nix = {
    buildMachines = builtins.filter (m: m.hostName != cfg.hostname) [
      {
        hostName = "opportunity";
        system = "x86_64-linux";
        maxJobs = 12;
        speedFactor = 2;
        supportedFeatures = [ "kvm" "big-parallel" ];
        sshUser = "alex";
        sshKey = "/home/alex/.ssh/${cfg.buildSshKey}";
      }
      {
        hostName = "spirit";
        system = "x86_64-linux";
        maxJobs = 8;
        speedFactor = 1;
        supportedFeatures = [ "kvm" ];
        sshUser = "alex";
        sshKey = "/home/alex/.ssh/${cfg.buildSshKey}";
      }
    ];

    distributedBuilds = true;
  };
}
