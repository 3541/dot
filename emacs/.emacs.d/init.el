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
  :ensure t)

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
  (setq evil-undo-system 'undo-tree)
  (global-undo-tree-mode))

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
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  (add-to-list
   'eglot-server-programs
   '(rust-mode . ("/home/alex/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/bin/rust-analyzer")))
  (add-to-list
   'eglot-server-programs
   '(python-mode . ("/home/alex/.local/bin/pyls")))
  (add-to-list 'eglot-server-programs '(swift-mode . ("sourcekit-lsp")))
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'java-mode-hook 'eglot-ensure)
  (add-hook 'rust-mode-hook 'eglot-ensure)
  (add-hook 'go-mode-hook 'eglot-ensure)
  (add-hook 'swift-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure)
  (define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions)
  (setcdr (assq 'java-mode eglot-server-programs) #'jdtls-contact))

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
  :ensure t
  :config
  (setq rust-rustfmt-bin "/home/alex/.cargo/bin/rustfmt"))

(use-package swift-mode
  :ensure t)

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
  (setq org-drill-add-random-noise-to-intervals-p t))

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

(use-package org-roam
  :hook
  (after-init . org-roam-mode)
  :ensure t
  :demand t
  :custom
  (org-roam-directory "~/org/roam")
  :bind
  (:map org-roam-mode-map
        (("C-c r r" . org-roam)
         ("C-c r f" . org-roam-find-file)
	 ("C-c r b" . org-roam-switch-to-buffer)
	 ("C-c r j" . org-roam-jump-to-index)
         ("C-c r g" . org-roam-graph))
        :map org-mode-map
        (("C-c r i" . org-roam-insert)))
  :config
;;  (global-set-key (kbd "C-c r r") 'org-roam)
  )

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
	      (pdf-tools-install)))
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

(setq-default display-line-numbers 'visual)
(setq display-line-numbers-type 'visual)
(global-display-line-numbers-mode)

(global-set-key
 (kbd "C-c d")
 (lambda () (interactive)
   (set-window-dedicated-p
    (selected-window)
    (if (window-dedicated-p (selected-window))
        nil t))))

(setq gdb-many-windows t)

(setq tab-width 4)
(setq c-basic-offset 4)
(setq c-default-style "linux")
(setq sgml-basic-offset 4)
(setq-default fill-column 100)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

(defconst cc-style
  '("cc-mode"
    (c-offsets-alist . ((innamespace . [0])))))

(c-add-style "my-cc" cc-style)
(add-hook 'c++-mode-hook #'(lambda () (c-set-style "my-cc")))

(add-to-list 'custom-theme-load-path "/home/alex/.emacs.d/emacs-color-theme-solarized")
(load-theme 'solarized t)
(setq frame-background-mode 'dark)
(mapc 'frame-set-background-mode (frame-list))
(enable-theme 'solarized)

(set-frame-font "Iosevka Term-14" nil t)
(add-to-list 'default-frame-alist '(font . "Iosevka Term-14"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(display-line-numbers 'relative)
 '(evil-undo-system 'undo-tree)
 '(helm-completion-style 'emacs)
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(swift-mode tree-sitter-langs go-mode yaml-mode company-org-roam yasnippet which-key use-package undo-tree solarized-theme rust-mode poly-R paredit org-roam org-pdftools org-drill org-download org-bullets moody meson-mode magit lsp-ui htmlize helm-projectile helm-lsp haskell-mode evil-org evil-collection ess eglot dtrt-indent dired-hide-dotfiles diff-hl deft deadgrep company cmake-mode buffer-move auto-package-update auctex))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
