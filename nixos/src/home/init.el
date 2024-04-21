(require 'package)

(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (auto-package-update-at-time "20:00"))

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(use-package evil
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
  :config
  (evil-collection-init
    '(cmake-mode company dired eglot flymake helm magit)))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package helm
  :demand t
  :bind
  (("M-x" . helm-M-x)
   ("C-c f" . helm-find-files))
  :config
  (helm-mode 1))

(require 'tramp)

(use-package company
  :config
  (setq company-idle-delay 0.0)
  (setq company-minimum-prefix-length 1)
  (setq company-frontends '(company-pseudo-tooltip-unless-just-one-frontend company-preview-frontend))
  (global-company-mode))

(use-package company-box
  :hook (company-mode . company-box-mode))

(defun my-projectile-project-find-function (dir)
  (let ((root (projectile-project-root dir)))
    (and root (cons 'transient root))))

(use-package projectile
  :after evil
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  ;;(define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)
  ;;(evil-define-key 'motion ag-mode-map (kbd "C-p") 'projectile-find-file)
  ;;(evil-define-key 'motion rspec-mode-map (kbd "C-p") 'projectile-find-file)
  (setq projectile-completion-system 'helm)
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-require-project-root t)
  (setq projectile-enable-caching t)
  (add-to-list 'project-find-functions 'my-projectile-project-find-function)
  (projectile-mode))

(use-package helm-projectile
  :after projectile helm
  :config
  (helm-projectile-on))

(defun find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'find-go-module)

(require 'bind-key)
(bind-key "C-c e a" 'eglot-code-actions)
(bind-key "C-c e f" 'eglot-format)
(bind-key "C-c e x" 'eglot-code-action-extract)
(bind-key "C-c e r" 'eglot-rename)

(add-hook 'c-ts-mode-hook 'eglot-ensure)
(add-hook 'c++-ts-mode-hook 'eglot-ensure)
(add-hook 'java-ts-mode-hook 'eglot-ensure)
(add-hook 'rust-ts-mode-hook 'eglot-ensure)
(add-hook 'go-ts-mode-hook 'eglot-ensure)
(add-hook 'swift-ts-mode-hook 'eglot-ensure)
(add-hook 'python-ts-mode-hook 'eglot-ensure)
(add-hook 'haskell-ts-mode-hook 'eglot-ensure)
(define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions)
(setq-default eglot-workspace-configuration
                '((:python (analysis . ((autoSearchPaths . t)
                                        (useLibraryCodeForTypes . t)
                                        (diagnosticMode . "workspace"))))))

;;(use-package tree-sitter-langs
;;  :ensure t)
;;(use-package tree-sitter
;;;;  :after tree-sitter-langs
;;  :config
;;  (add-hook 'c-mode-hook 'tree-sitter-hl-mode)
;;  (add-hook 'c++-mode-hook 'tree-sitter-hl-mode)
;;  (add-hook 'java-mode-hook 'tree-sitter-hl-mode)
;;  (add-hook 'rust-mode-hook 'tree-sitter-hl-mode)
;;  (add-hook 'go-mode-hook 'tree-sitter-hl-mode)
;;  (add-hook 'swift-mode-hook 'tree-sitter-hl-mode)
;;  (add-hook 'python-mode-hook 'tree-sitter-hl-mode)
;;  (add-hook 'sh-mode-hook 'tree-sitter-hl-mode))

(use-package yasnippet
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

;;(use-package typescript-mode
;;;;  :config
;;  (add-hook 'typescript-mode-hook (lambda ()
;;                                    (tide-setup)
;;                                    (flycheck-mode)))
;;  (add-to-list 'auto-mode-alist '("\\.js\\'" . typescript-mode))
;;  (add-to-list 'auto-mode-alist '("\\.mjs\\'" . typescript-mode)))

(use-package org
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
  :after org
  :config
  (setq org-drill-scope 'directory)
  (setq org-drill-add-random-noise-to-intervals-p t)
  (setq org-drill-adjust-intervals-for-early-and-late-repetitions-p t))

(use-package evil-org
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
	    (lambda ()
	      (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-bullets
  :after org
  :config
  (add-hook 'org-mode-hook
	    (lambda ()
	      (org-bullets-mode 1))))

(setq org-roam-v2-ack t)
(use-package org-roam
  :hook
  (after-init . org-roam-mode)
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
  (org-roam-db-autosync-mode))

(use-package org-roam-protocol
  :ensure org-roam
  :after org-roam)

(use-package org-pdftools
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
  :after org
  :bind
  (:map org-mode-map
        (("C-c y" . org-download-screenshot)))
  :config
  (setq-default org-download-image-dir "~/org/roam/img")
  (setq org-download-screenshot-method "scrot -s %s"))

(use-package pdf-tools
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
  :config
  (dtrt-indent-global-mode t))

(use-package diff-hl
  :config
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

(use-package magit
  :bind
  ("C-x g" . magit-status)
  :config
  (use-package with-editor
    :ensure t)
  (add-hook 'with-editor-mode-hook 'evil-insert-state))

(use-package subword
  :config
  (global-subword-mode 1))

(use-package paredit
  :config
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'lisp-mode-hook 'paredit-mode)
  (add-hook 'scheme-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode))

(use-package htmlize
  :ensure t)

(use-package flyspell
  :config
  (setq ispell-list-command "--list")
  (add-hook 'text-mode-hook 'turn-on-auto-fill)
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'git-commit-mode-hook 'flyspell-mode))

(use-package dired-hide-dotfiles
  :config
  (dired-hide-dotfiles-mode)
  (define-key dired-mode-map "." 'dired-hide-dotfiles-mode))

(use-package async
  :config
  (dired-async-mode 1))

(use-package which-key
  :config
  (which-key-mode))

(use-package buffer-move
  :config
  (global-set-key (kbd "C-S-h") 'buf-move-left)
  (global-set-key (kbd "C-S-j") 'buf-move-down)
  (global-set-key (kbd "C-S-k") 'buf-move-up)
  (global-set-key (kbd "C-S-l") 'buf-move-right))

(use-package cmake-mode
  :config
  (setq cmake-tab-width 4))

(use-package meson-mode
  :ensure t)

(use-package haskell-mode
  :ensure t)

(use-package ess
  :init (require 'ess-site))

(use-package yaml-mode
  :ensure t)

(use-package terraform-mode
  :ensure t)

(use-package sql-indent
  :config
  (add-hook 'sql-mode-hook 'sqlind-minor-mode))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package direnv
  :config
  (add-hook 'prog-mode-hook 'direnv-mode))

(use-package bazel
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package nasm-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.asm\\'" . nasm-mode)))

(use-package protobuf-mode
  :ensure t)

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package yapfify
  :bind
  (("C-c y" . yapfify-region)))

(use-package jinja2-mode
  :ensure t)

(use-package stgit
  :ensure t)

(use-package magit-stgit
  :ensure t)

(use-package markdown-mode
  :config
  (setq markdown-command '("@pandoc@" "--from=markdown" "--to=html5")))

;;(use-package activity-watch-mode
;;;;  :config
;;  (global-activity-watch-mode))

(use-package ein
  :ensure t)

(use-package python-black
  :bind
  (("C-c b" . python-black-buffer))
  :config
  (setq python-black-extra-args '("--line-length=100")))

;;(use-package copilot
;;  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
;;;;  :config
;;  (setq copilot-node-executable "${pkgs.nodejs}/bin/node")
;;  (add-hook 'prog-mode-hook 'copilot-mode))

(use-package tuareg
  :ensure t)

(use-package dune
  :ensure t)

(use-package merlin
  :config
  (add-hook 'tuareg-mode-hook #'merlin-mode)
  (add-hook 'merlin-mode-hook #'company-mode)
  (setq merlin-error-after-save nil))

(use-package merlin-eldoc
  :hook ((tuareg-mode) . merlin-eldoc-setup))

(use-package flycheck-ocaml
  :config
  (flycheck-ocaml-setup))

(use-package utop
  :config
  (add-hook 'tuareg-mode-hook #'utop-minor-mode))

(use-package ocamlformat
  :config
  (add-hook 'tuareg-mode-hook
	    (lambda ()
	      (add-hook 'before-save-hook 'ocamlformat-before-save))))
(use-package fzf
  :after evil
  :bind
  (("C-p" . fzf-projectile))
  :config
  (define-key evil-normal-state-map (kbd "C-p") 'fzf-projectile)
  (evil-define-key 'normal ag-mode-map (kbd "C-p") 'fzf-projectile)
  (evil-define-key 'normal dired-mode-map (kbd "C-p") 'fzf-projectile)
  (evil-define-key 'normal rspec-mode-map (kbd "C-p") 'fzf-projectile))

(use-package helm-rg
  :bind
  ("C-c g" . helm-rg))

(defun switch-themes (sun-event &optional first-run)
  "Switch themes on sunrise and sunset."
  (if first-run                         ; set theme on initialization
      (cond ((memq sun-event '(sunrise midday))
      	     (load-theme 'solarized-light t))
            ((memq sun-event '(sunset midnight))
             (load-theme 'solarized-dark t)))
    (cond ((eq sun-event 'sunrise)
      	   (load-theme 'solarized-light t))
          ((eq sun-event 'sunset)
           (load-theme 'solarized-dark t)))))

(use-package rase
  :after solarized-theme
  :config
  (add-hook 'rase-functions 'switch-themes)
  (setq calendar-location-name "Sydney, NSW")
  (setq calendar-latitude 33.87)
  (setq calendar-longitude 151.21)
  (rase-start t))

(use-package windresize
  :bind (("C-c w" . windresize)
         :map windresize-map
         ("h" . windresize-left)
         ("j" . windresize-down)
         ("k" . windresize-up)
         ("l" . windresize-right)))

(use-package vterm
  :config
  (add-hook 'vterm-mode-hook
  		(lambda ()
  		(setq-local evil-insert-state-cursor 'box)
  		(evil-insert-state)))
  (define-key vterm-mode-map [return]                      #'vterm-send-return)
  (setq vterm-keymap-exceptions nil)
  (evil-define-key 'insert vterm-mode-map (kbd "C-e")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-f")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-a")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-v")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-b")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-w")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-u")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-n")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-m")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-p")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-j")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-k")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-t")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-g")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd ",c")       #'multi-vterm)
  (evil-define-key 'normal vterm-mode-map (kbd ",n")       #'multi-vterm-next)
  (evil-define-key 'normal vterm-mode-map (kbd ",p")       #'multi-vterm-prev)
  (evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume))

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
(pixel-scroll-precision-mode 1)

(global-hl-line-mode)

(setq compilation-scroll-output t)

(setq initial-major-mode 'org-mode)

(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)

(setq dired-recursive-deletes 'top)

(evil-define-key 'normal dired-mode-map (kbd "j") 'dired-next-line)
(evil-define-key 'normal dired-mode-map (kbd "k") 'dired-previous-line)

(setq-default indent-tabs-mode nil)

(setq-default display-line-numbers 'relative)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(add-to-list 'auto-mode-alist '("\\.pro\\'" . prolog-mode))
(add-to-list 'auto-mode-alist '("\\.ccm\\'" . c++-mode))

(global-set-key
 (kbd "C-c d")
 (lambda () (interactive)
   (set-window-dedicated-p
    (selected-window)
    (if (window-dedicated-p (selected-window))
        nil t))))

;;(defun my-tab ()
;;  (interactive)
;;  (or (copilot-accept-completion)
;;      (company-complete)
;;      (indent-for-tab-command)))

(define-key prog-mode-map (kbd "C-c e n") 'flymake-goto-next-error)
(define-key prog-mode-map (kbd "C-c e p") 'flymake-goto-prev-error)
(define-key prog-mode-map (kbd "C-c o") 'ff-get-other-file)
(define-key prog-mode-map (kbd "C-c C-c") 'comment-or-uncomment-region)
;;(define-key prog-mode-map (kbd "<tab>") #'my-tab)
(defun flymake-checkstyle-java-init ()
  (let* ((tmp (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
         (local (file-relative-name tmp (file-name-directory buffer-file-name))))
        (list "checkstyle"
          (list "-c"
            (concat (file-name-as-directory (projectile-project-root)) "sputnik-rules/checkstyle.xml")
            local))))
(add-to-list 'flymake-err-line-patterns
  '("\\(\\w+.java\\):\\([0-9]+\\):[0-9]*[:, ]*\\(.+\\)" 1 2 nil 3))
(add-to-list 'flymake-allowed-file-name-masks '("\\.java$"
             flymake-checkstyle-java-init))
(setq-default ff-other-file-alist '(("\\.cc\\'" (".hh" ".ccm"))
                                    ("\\.hh\\'" (".cc"))
                                    ("\\.hpp\\'" (".cpp"))
                                    ("\\.cpp\\'" (".hpp"))
                                    ("\\.c\\'" (".h"))
                                    ("\\.h\\'" (".c"))
                                    ("\\.ccm\\'" (".cc"))))
(add-hook 'find-file-hook 'flymake-mode)

(setq gdb-many-windows t)

(setq-default tab-width 4)
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

(setq frame-inhibit-implied-resize t)
(mapc 'frame-set-background-mode (frame-list))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
      (load custom-file))
(xterm-mouse-mode)

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

;; (setq treesit-font-lock-level 4)
(use-package solarized-theme
  :config
  (setq solarized-use-more-italic t)
  (load-theme 'solarized-dark t))

(set-frame-font "@font@" nil t)
(add-to-list 'default-frame-alist '(font . "@font@"))
