; ---Sensible default behaviors---
(load-file "~/.emacs.d/sensible-defaults.el")
(sensible-defaults/use-all-settings)
(setq backup-by-copying t
      create-lockfiles nil
      backup-directory-alist '(("." . "~/.cache/emacs-backups"))
      auto-save-file-name-transforms '((".*" "~/.cache/emacs-backups/" t)))
(xterm-mouse-mode)
(setq warning-minimum-level 'error)

; ---Packaging config---
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(if init-file-debug
  (setq use-package-verbose t
        use-package-expand-minimally nil
        use-package-compute-statistics t
        debug-on-error t)
(setq use-package-verbose nil
      use-package-expand-minimally t))

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (auto-package-update-at-time "20:00"))

; ---Text editing---
(setq-default indent-tabs-mode nil)

(setq-default display-line-numbers 'relative)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(setq-default tab-width 4)
(setq c-basic-offset 4)
(setq c-default-style "linux")
(setq sgml-basic-offset 4)
(setq-default fill-column 100)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)

(show-paren-mode t)
(column-number-mode)

(setq major-mode-remap-alist
 '((c-mode . c-ts-mode)
   (c++-mode . c++-ts-mode)
   (c-or-c++-mode . c-or-c++-ts-mode)
   (rust-mode . rust-ts-mode)
   (json-mode . json-ts-mode)
   (python-mode . python-ts-mode)
   (yaml-mode . yaml-ts-mode)
   (bash-mode . bash-ts-mode)
   (go-mode . go-ts-mode)
   (haskell-mode . haskell-ts-mode)
   (java-mode . java-ts-mode)))

; ---Navigation---
(use-package helm
  :demand t
  :bind
  (("M-x" . helm-M-x)
   ("C-c f" . helm-find-files))
  :config
  (require 'helm-autoloads)
  (helm-mode 1))

(setq project-mode-line t)
(global-set-key (kbd "C-c f") 'project-find-file)

(global-subword-mode)

; ---Appearance---
(set-frame-font "@font@" nil t)
(add-to-list 'default-frame-alist '(font . "@font@"))

(tool-bar-mode -1)
(set-window-scroll-bars (minibuffer-window) nil nil)
(set-scroll-bar-mode nil)
(toggle-scroll-bar -1)

(global-prettify-symbols-mode t)

(setq scroll-step 1)
(setq scroll-margin 1)
(setq scroll-conservatively 100)
(pixel-scroll-precision-mode 1)

(global-hl-line-mode)

(use-package solarized-theme
  :config
  (setq solarized-use-more-italic t)
  (load-theme 'solarized-dark t))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

; ---Helix---
(use-package multiple-cursors)

(use-package helix
  :demand t
  :after multiple-cursors
  :config
  (helix-multiple-cursors-setup)
  (helix-mode)
  (helix-jj-setup 0.2)
  (helix-define-key 'normal "m m" #'evilmi-jump-items-native))

(use-package evil-matchit
  :defer t)

; ---Language support---
(use-package nix-mode
  :mode "\\.nix\\'"
  :config
  (setq nix-nixfmt-bin "@nixfmt@")
  (define-key nix-mode-map (kbd "C-x f") 'nix-format-buffer))

; ---Miscellaneous other packages---
(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(use-package flyspell
  :hook
  (text-mode . flyspell-mode)
  (org-mode . flyspell-mode)
  (git-commit-mode . flyspell-mode)
  :config
  (setq ispell-program-name "@ispell@"))

(use-package which-key
  :config
  (which-key-mode))
