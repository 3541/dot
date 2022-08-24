{ cfg, lib, pkgs, ... }: {
  config = lib.mkIf (cfg.enable && cfg.home.enable) {
    home.packages = lib.optional (cfg.role == "workstation") pkgs.stgit;

    programs = {
      gh.enable = cfg.role == "workstation";

      git = {
        enable = true;
        package = if cfg.role == "workstation" then pkgs.gitAndTools.gitFull else pkgs.git;
        userName = "Alex O'Brien";
        userEmail = "3541@3541.website";
        ignores = [ ".DS_Store" ".direnv" ".envrc" ];

        aliases = {
          d = "diff";
          ds = "diff --staged";
          a = "add";
          c = "commit";
          p = "push";
          pl = "pull";
          cl = "clone";
          s = "status";
          l = "log";
          r = "rebase";
          f = "fetch";
        };

        extraConfig = {
          submodule.fetchJobs = 8;
          log.showSignature = true;
          init.defaultBranch = "trunk";
          merge.renamelimit = 10000;
          rebase.autoSquash = true;
          format.signOff = true;

          pull = {
            rebase = false;
            ff = "only";
          };

          sendemail = {
            smtpserver = "smtp.mail.us-west-2.awsapps.com";
            smtpuser = "3541@3541.website";
            smtpencryption = "ssl";
            smtpserverport = 465;
          };
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
