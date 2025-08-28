{
  config,
  lib,
  pkgs,
  ...
}:
let cfg = config.a3; in
{
  programs = {
    git = {
      enable = true;
      package = if cfg.system.role == "workstation" then pkgs.gitAndTools.gitFull else pkgs.git;

      userName = cfg.user.fullName;
      userEmail = cfg.user.email;

      signing = {
        signByDefault = true;
        format = "ssh";
        key = "key::sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAIL8q7pP1kNhpZVn1msZaW5sua3Uh2gIg9EX8hcR6fbR0AAAABHNzaDo=";
      };

      ignores = [
        ".DS_Store"
        ".direnv"
      ];

      delta = {
        enable = true;

        options = {
          features = "side-by-side line-numbers";
          syntax-theme = "Solarized (dark)";
          whitespace-error-style = "22 reverse";
        };
      };

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
        init.defaultBranch = "trunk";
        submodule.fetchJobs = 8;
        gpg.ssh.allowedSignersFile = "${toString cfg.user.home}/.config/git/allowed_signers";

        pull = {
          ff = "only";
          rebase = false;
        };

        log = {
          showSignature = true;
          date = "local";
        };
      };
    };
  };

  home.file.".config/git/allowed_signers".source = ../../home/git/dot-config/git/allowed_signers;
}
