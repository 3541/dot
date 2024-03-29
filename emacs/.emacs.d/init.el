;; Copied from Nix-generated version.

(require 'package)

(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (auto-package-update-at-time "12:00"))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode t)
  (define-key evil-motion-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-motion-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-motion-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-motion-state-map (kbd "C-l") 'evil-window-right)
  (setq x-select-enable-clipboard nil)
  (evil-set-undo-system 'undo-tree))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init
   '(cmake-mode company dired eglot flymake helm magit)))

(use-package helm
  :ensure t
  :demand t
  :preface (require 'helm-config)
  :bind
  (("M-x" . helm-M-x)
   ("C-c f" . helm-find-files))
  :config
  (helm-mode 1))
(require 'tramp)

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.0)
  (setq company-minimum-prefix-length 1)
  (global-company-mode))

(defun my-projectile-project-find-function (dir)
  (let ((root (projectile-project-root dir)))
    (and root (cons 'transient root))))

(use-package projectile
  :ensure t
  :after evil
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)
  (evil-define-key 'motion ag-mode-map (kbd "C-p") 'projectile-find-file)
  (evil-define-key 'motion rspec-mode-map (kbd "C-p") 'projectile-find-file)

  (setq projectile-completion-system 'helm)
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-require-project-root nil)
  (setq projectile-enable-caching t)
  (add-to-list 'project-find-functions 'my-projectile-project-find-function)
  (projectile-mode))

(use-package helm-projectile
  :ensure t
  :after projectile helm
  :config
  (helm-projectile-on))

(defconst jdtls-home "/opt/jdtls/plugins/org.eclipse.equinox.launcher_1.6.100.v20201223-0822.jar")
(defun jdtls-contact (interactive)
  (let ((cp (getenv "CLASSPATH")))
    (setenv "CLASSPATH" (concat cp ":" jdtls-home))
    (unwind-protect
        (eglot--eclipse-jdt-contact nil)
      (setenv "CLASSPATH" cp))))

(defun find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'find-go-module)

(use-package eglot
  :ensure t
  :demand t
  :bind
  (("C-c e r" . eglot-rename))
  (("C-c e a" . eglot-code-actions))
  :config
  (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  (add-to-list
   'eglot-server-programs
   '(python-mode . ("~/.local/bin/pyls")))
  (add-to-list 'eglot-server-programs '(swift-mode . ("sourcekit-lsp")))
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'java-mode-hook 'eglot-ensure)
  (add-hook 'rust-mode-hook 'eglot-ensure)
  (add-hook 'go-mode-hook 'eglot-ensure)
  (add-hook 'swift-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'haskell-mode-hook 'eglot-ensure)
  (define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions)
  (setcdr (assq 'java-mode eglot-server-programs) #'jdtls-contact)
  (defun eglot-format-buffer-on-save ()
    (if (and (project-current) (eglot-managed-p))
        (add-hook 'before-save-hook #'eglot-format-buffer nil 'local)
      (remove-hook 'before-save-hook #'eglot-format-buffer 'local)))
  (add-hook 'eglot-managed-mode-hook #'eglot-format-buffer-on-save))

(use-package tree-sitter-langs
  :ensure t)
(use-package tree-sitter
  :ensure t
  :after tree-sitter-langs
  :config
  (add-hook 'c-mode-hook 'tree-sitter-hl-mode)
  (add-hook 'c++-mode-hook 'tree-sitter-hl-mode)
  (add-hook 'java-mode-hook 'tree-sitter-hl-mode)
  (add-hook 'rust-mode-hook 'tree-sitter-hl-mode)
  (add-hook 'go-mode-hook 'tree-sitter-hl-mode)
  (add-hook 'swift-mode-hook 'tree-sitter-hl-mode)
  (add-hook 'python-mode-hook 'tree-sitter-hl-mode))

(use-package yasnippet
  :ensure t
  :hook
  (rust-mode . yas-minor-mode)
  :config
  (yas-global-mode t))

(use-package go-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package swift-mode
  :ensure t)

(use-package tide
  :ensure t)

(use-package web-mode
  :ensure t
  :hook (web-mode . (lambda ()
                      (when (string-equal "tsx" (file-name-extension buffer-file-name))
                        (tide-setup))))
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode)))

(use-package typescript-mode
  :ensure t
  :config
  (add-hook 'typescript-mode-hook (lambda ()
                                    (tide-setup)
                                    (flycheck-mode)))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . typescript-mode))
  (add-to-list 'auto-mode-alist '("\\.mjs\\'" . typescript-mode)))

(use-package org
  :ensure t
  :config
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c s") 'org-search-view)
  (setq org-hide-leading-stars t)
  (setq org-log-done 'time)
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-directory "~/org/roam")
  (setq org-index-file "~/org/roam/index.org")
  (setq org-agenda-files (list org-directory))
  (setq org-agenda-span 14)
  (setq org-startup-indented t)
  (setq org-pretty-entities t)
  (setq org-latex-packages-alist '(("margin=2.5cm" "geometry" nil)))
  (setq org-duration-format '(hh:mm))
  (setq org-use-sub-superscripts "{}")
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.5))
  (with-eval-after-load 'org (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
        (C . t)
        (python . t)
        (ruby . t))))
  (org-reload))

(use-package org-drill
  :ensure t
  :after org
  :config
  (setq org-drill-scope 'directory)
  (setq org-drill-add-random-noise-to-intervals-p t)
  (setq org-drill-adjust-intervals-for-early-and-late-repetitions-p t))

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
	    (lambda ()
	      (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-bullets
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook
	    (lambda ()
	      (org-bullets-mode 1))))

(setq org-roam-v2-ack t)
(use-package org-roam
  :hook
  (after-init . org-roam-mode)
  :ensure t
  :demand t
  :custom
  (org-roam-directory "~/org/roam")
  :bind (("C-c r r" . org-roam)
         ("C-c r f" . org-roam-node-find)
	     ("C-c r b" . org-roam-switch-to-buffer)
	     ("C-c r j" . org-roam-jump-to-index)
         ("C-c r g" . org-roam-graph)
         ("C-c r i" . org-roam-node-insert))
  :config
  (org-roam-setup))

(use-package org-roam-protocol
  :ensure org-roam
  :after org-roam)

(use-package org-pdftools
  :ensure t
  :after org
  :config
  (setq org-pdftools-root-dir "~/org/roam/doc")
  (with-eval-after-load 'org
    (org-link-set-parameters "pdftools"
                             :follow #'org-pdftools-open
                             :complete #'org-pdftools-complete-link
                             :store #'org-pdftools-store-link
                             :export #'org-pdftools-export)
    (add-hook 'org-store-link-functions 'org-pdftools-store-link)))

(use-package org-download
  :ensure t
  :after org
  :bind
  (:map org-mode-map
        (("C-c y" . org-download-screenshot)))
  :config
  (setq-default org-download-image-dir "~/org/roam/img")
  (setq org-download-screenshot-method "scrot -s %s"))

(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-tools-install)
  :defer t
  :config
  (evil-set-initial-state 'pdf-view-mode 'normal))

(use-package org-noter
  :ensure t)

(use-package tex-site
  :ensure auctex
  :after pdf-tools
  :config
  (add-hook 'LaTeX-mode-hook
	    (lambda ()
	      (setq TeX-PDF-mode t)
	      (setq TeX-source-correlate-method 'synctex)
	      (setq TeX-source-correlate-start-server t)
	      (pdf-tools-install)
        (LaTeX-math-mode)))
  (setq TeX-engine 'xetex)
  (add-hook 'TeX-after-TeX-LaTeX-command-finished-hook
	    #'TeX-revert-document-buffer)
  (setq TeX-view-program-selection '((output-pdf "pdf-tools"))
	TeX-source-correlate-start-server t)
  (setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-cite-format "\\parencite[]{%l}"))

(use-package dtrt-indent
  :ensure t)

(use-package diff-hl
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status)

  :config
  (use-package with-editor
    :ensure t)
  (add-hook 'with-editor-mode-hook 'evil-insert-state))

(use-package subword
  :ensure t
  :config
  (global-subword-mode 1))

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'lisp-mode-hook 'paredit-mode)
  (add-hook 'scheme-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode))

(use-package htmlize
  :ensure t)

(use-package flyspell
  :ensure t
  :config
  (setq ispell-list-command "--list")
  (add-hook 'text-mode-hook 'turn-on-auto-fill)
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'git-commit-mode-hook 'flyspell-mode))

(use-package dired-hide-dotfiles
  :ensure t
  :config
  (dired-hide-dotfiles-mode)
  (define-key dired-mode-map "." 'dired-hide-dotfiles-mode))

(use-package async
  :ensure t
  :config
  (dired-async-mode 1))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package buffer-move
  :ensure t
  :config
  (global-set-key (kbd "C-S-h") 'buf-move-left)
  (global-set-key (kbd "C-S-j") 'buf-move-down)
  (global-set-key (kbd "C-S-k") 'buf-move-up)
  (global-set-key (kbd "C-S-l") 'buf-move-right))

(use-package cmake-mode
  :ensure t)

(use-package meson-mode
  :ensure t)

(use-package haskell-mode
  :ensure t)

(use-package ess
  :ensure t
  :init (require 'ess-site))

(use-package poly-R
  :after ess
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package terraform-mode
  :ensure t)

(use-package sql-indent
  :ensure t
  :config
  (add-hook 'sql-mode-hook 'sqlind-minor-mode))

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package direnv
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'direnv-mode))

(use-package bazel
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package nasm-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.asm\\'" . nasm-mode)))

(load-file "~/.emacs.d/sensible-defaults.el")

(sensible-defaults/use-all-settings)
(setq backup-by-copying t
      create-lockfiles nil
      backup-directory-alist '(("." . "~/.cache/emacs-backups"))
      auto-save-file-name-transforms '((".*" "~/.cache/emacs-backups/" t)))

(tool-bar-mode -1)
(set-window-scroll-bars (minibuffer-window) nil nil)
(set-scroll-bar-mode nil)
(toggle-scroll-bar -1)

(global-prettify-symbols-mode t)

(setq scroll-step 1)
(setq scroll-margin 1)
(setq scroll-conservatively 100)

(global-hl-line-mode)

(setq compilation-scroll-output t)

(setq initial-major-mode 'org-mode)

(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)

(setq dired-recursive-deletes 'top)

(evil-define-key 'normal dired-mode-map (kbd "j") 'dired-next-line)
(evil-define-key 'normal dired-mode-map (kbd "k") 'dired-previous-line)

(setq-default indent-tabs-mode nil)

(setq-default display-line-numbers t)
(setq display-line-numbers-type t)
(global-display-line-numbers-mode)

(add-to-list 'auto-mode-alist '("\\.pro\\'" . prolog-mode))

(global-set-key
 (kbd "C-c d")
 (lambda () (interactive)
   (set-window-dedicated-p
    (selected-window)
    (if (window-dedicated-p (selected-window))
        nil t))))

(define-key prog-mode-map (kbd "C-c e n") 'flymake-goto-next-error)
(define-key prog-mode-map (kbd "C-c e p") 'flymake-goto-prev-error)

(setq gdb-many-windows t)

(setq tab-width 4)
(setq c-basic-offset 4)
(setq c-default-style "linux")
(setq sgml-basic-offset 4)
(setq-default fill-column 100)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)

(show-paren-mode t)
(column-number-mode)

(defconst cc-style
  '("cc-mode"
    (c-offsets-alist . ((innamespace . [0])))))

(c-add-style "my-cc" cc-style)
(add-hook 'c++-mode-hook #'(lambda () (c-set-style "my-cc")))

(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized")
(load-theme 'solarized t)
(setq frame-background-mode 'dark)
(mapc 'frame-set-background-mode (frame-list))
(enable-theme 'solarized)

(set-frame-font "Iosevka Custom-14.000000" nil t)
(add-to-list 'default-frame-alist '(font . "Iosevka Custom-14.000000"))
