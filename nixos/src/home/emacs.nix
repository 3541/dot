{ cfg, lib, pkgs, emacs-sensible-defaults, ... }: {
  config =
    lib.mkIf (cfg.enable && cfg.home.enable && cfg.role == "workstation") {
      home = {
        packages = [ pkgs.nixfmt ]; # For nix-format-buffer.
        sessionVariables.EDITOR = "emacsclient -c";
      };

      services.emacs = lib.mkIf (cfg.platform != "darwin") {
        enable = true;
        client.enable = true;
      };

      programs.emacs = {
        enable = true;
        extraPackages = (epkgs:
          (with epkgs; [ emacsql-sqlite treesit-grammars.with-all-grammars ]));
        package = pkgs.emacs29;
      };

      home.file = {
        sensibleDefaults = {
          source = emacs-sensible-defaults + "/sensible-defaults.el";
          target = ".emacs.d/sensible-defaults.el";
        };

        init = {
          target = ".emacs.d/init.el";
          text = (builtins.readFile (pkgs.substituteAll {
            src = ./init.el;
            pandoc = "${pkgs.pandoc}/bin/pandoc";
            font = "${cfg.home.ui.fonts.editor.font}-${
                toString cfg.home.ui.fonts.editor.size
              }";
          })) + lib.optionalString (cfg.platform == "darwin") ''
            (defun set-exec-path-from-shell-PATH ()
              "Set up Emacs' `exec-path' and PATH environment variable to match
              that used by the user's shell.

              This is particularly useful under Mac OS X and macOS, where GUI
              apps are not started from a shell."
              (interactive)
              (let ((path-from-shell (replace-regexp-in-string
                                       "[ \t\n]*$" "" (shell-command-to-string
                                                       "bash --login -c 'echo $PATH'"))))
               (setenv "PATH" path-from-shell)
               (setq exec-path (split-string path-from-shell path-separator))))

            (set-exec-path-from-shell-PATH)'';
        };
      };
    };
}
