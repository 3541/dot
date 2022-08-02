{ config, lib, ... }:
let cfg = config.a3;
in {
  options.a3.build = {
    distributed = lib.mkEnableOption "Enable distributed builds.";

    sshKey = lib.mkOption {
      type = lib.types.str;
      default = "id_ed25519";
    };
  };

  config.nix = lib.mkIf (cfg.enable && cfg.build.distributed) {
    buildMachines = builtins.filter (m: m.hostName != cfg.hostName)
      (builtins.map (s:
        s // {
          sshUser = "alex";
          sshKey = "/home/alex/.ssh/${cfg.build.sshKey}";
        }) [
          {
            hostName = "opportunity";
            systems = [ "x86_64-linux" "i686-linux" ];
            maxJobs = 12;
            speedFactor = 2;
            supportedFeatures = [ "kvm" "big-parallel" ];
          }
          {
            hostName = "spirit";
            systems = [ "x86_64-linux" "i686-linux" ];
            maxJobs = 8;
            speedFactor = 1;
            supportedFeatures = [ "kvm" ];
          }
          {
            hostName = "sagittarius";
            systems = [ "x86_64-linux" "i686-linux" ];
            maxJobs = 8;
            speedFactor = 0;
            supportedFeatures = [ "kvm" ];
          }
        ]);

    distributedBuilds = true;
  };
}
