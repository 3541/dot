{
  package-inputs,
  pkgs,
  ...
}:
{
  programs.helix = {
    enable = true;
    package = package-inputs.helix;
    defaultEditor = true;

    settings = {
      theme = "solarized_light";

      editor = {
        bufferline = "multiple";
        color-modes = true;
        cursorline = true;
        line-number = "relative";
        popup-border = "all";
        scrolloff = 3;
        rainbow-brackets = true;
        true-color = true;
        cursor-shape.insert = "bar";

        lsp = {
          display-inlay-hints = true;
          display-messages = true;
        };

        statusline = {
          left = [
            "mode"
            "spinner"
            "file-name"
            "read-only-indicator"
            "file-modification-indicator"
            "register"
          ];
          right = [
            "version-control"
            "diagnostics"
            "selections"
            "register"
            "position"
            "file-encoding"
          ];
        };

        auto-pairs = {
          "\"" = "\"";
          "'" = "'";
          "(" = ")";
          "[" = "]";
          "`" = "`";
          "{" = "}";
        };
      };

      keys.normal = {
        "A-," = "jump_backward";
        "A-." = "goto_definition";
        "A->" = "jump_forward";
        "A-m" = "remove_primary_selection";
      };
    };

    languages.language = [
      {
        name = "nix";
        scope = "source.nix";
        file-types = [ "nix" ];
        formatter.command = "${pkgs.nixfmt-rfc-style}/bin/nixfmt";
      }
      {
        name = "starlark";
        scope = "source.bazel";
        file-types = [ "bazel" "bzl" ];
        formatter.command = "${pkgs.buildifier}/bin/buildifier";
      }
    ];
  };

  home.file = {
    ".config/helix/init.scm".text = ''
      (require "cogs/keymaps.scm")
      (add-global-keybinding (hash "normal" (hash "C-o" ":alt")))
    '';

    ".config/helix/helix.scm".source = ../../home/helix/dot-config/helix/helix.scm;
    ".config/helix/cogs/keymaps.scm".source = ../../home/helix/dot-config/helix/cogs/keymaps.scm;
  };
}
