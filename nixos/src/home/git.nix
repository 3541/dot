{ cfg, lib, pkgs, ... }: {
  config = lib.mkIf (cfg.enable && cfg.home.enable) {
    home.packages = lib.optional (cfg.role == "workstation") pkgs.stgit;

    programs = {
      gh.enable = cfg.role == "workstation";

      git = {
        enable = true;
        userName = "Alex O'Brien";
        userEmail = "3541@3541.website";
        ignores = [ ".DS_Store" ".direnv" ];

        aliases = {
          d = "diff";
          ds = "diff --staged";
          a = "add";
          c = "commit";
          p = "push";
          cl = "clone";
          s = "status";
          l = "log";
          r = "rebase";
          ri = "rebase -i";
        };

        extraConfig = {
          submodule.fetchJobs = 8;
          pull = {
            rebase = false;
            ff = "only";
          };
          log.showSignature = true;
          init.defaultBranch = "trunk";
          merge.renamelimit = 10000;
          rebase.autoSquash = true;
        };

        signing = lib.mkIf (cfg.role == "workstation") {
          key = "0x1EECFF9EE39ED7AA";
          signByDefault = true;
        };

        delta = lib.mkIf (cfg.role == "workstation") {
          enable = true;
          options = {
            features = "side-by-side line-numbers";
            syntax-theme = "Solarized (dark)";
            whitespace-error-style = "22 reverse";
          };
        };
      };
    };
  };
}
