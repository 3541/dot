;; heavily based on https://github.com/hrs/dotfiles/blob/master/emacs/.emacs.d/configuration.org

;;(require 'package)

;;(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
;;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)
(package-initialize)

(straight-use-package 'use-package)

(use-package evil
  :straight t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode t)
  (define-key evil-motion-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-motion-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-motion-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-motion-state-map (kbd "C-l") 'evil-window-right))

(use-package evil-collection
  :straight t
  :after evil)

(use-package solarized-theme
  :straight t
  :config
  (setq solarized-use-variable-pitch nil
	solarized-scale-org-headlines nil)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9"))
  (load-theme 'solarized-dark t))

(use-package moody
  :straight t
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
;;  (moody-replace-vc-mode)
  )

(use-package company
  :straight t
  :config
  (setq company-idle-delay 0.2)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package projectile
  :straight t
  :after helm evil
  :bind
  ("C-c v" . deadgrep)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)
  (evil-define-key 'motion ag-mode-map (kbd "C-p") 'projectile-find-file)
  (evil-define-key 'motion rspec-mode-map (kbd "C-p") 'projectile-find-file)

  (setq projectile-completion-system 'helm)
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-require-project-root nil)
  (projectile-mode))

(use-package helm-projectile
  :straight t
  :after projectile
  :config
  (helm-projectile-on))

(use-package rust-mode
  :straight t
  :config
  (setq rust-format-on-save t))

(use-package eglot
  :straight t
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  (add-hook 'rust-mode-hook 'eglot-ensure)
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions))

(use-package org
  :straight t
  :config
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)
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
  (with-eval-after-load 'org (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
        (C . t)
        (python . t)
        (ruby . t))))
  (org-reload))

(use-package evil-org
  :straight t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
	    (lambda ()
	      (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-bullets
  :straight t
  :after org
  :config
  (add-hook 'org-mode-hook
	    (lambda ()
	      (org-bullets-mode 1))))

(use-package org-roam
  :hook
  (after-init . org-roam-mode)
  :straight (:host github :repo "jethrokuan/org-roam")
  :custom
  (org-roam-directory "~/org/roam")
  :bind
  (:map org-roam-mode-map
        (("C-c r r" . org-roam)
         ("C-c r f" . org-roam-find-file)
         ("C-c r g" . org-roam-show-graph))
        :map org-mode-map
        (("C-c r i" . org-roam-insert)))
  :config
;;  (global-set-key (kbd "C-c r r") 'org-roam)
  )

(use-package company-org-roam
  :straight t
  :after org-roam company org
  :config
  (company-org-roam-init))

(use-package org-pdftools
  :straight (:host github :repo "fuxialexander/org-pdftools" :branch "master")
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
  :straight t
  :after org
  :bind
  (:map org-mode-map
        (("C-c y" . org-download-screenshot)))
  :config
  (setq-default org-download-image-dir "~/org/roam/img")
  (setq org-download-screenshot-method "scrot -s %s"))

(use-package deft
  :straight t
  :after org
  :bind
  ("C-c r d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/org/roam"))

(use-package pdf-tools
  :straight t
  :mode ("\\.pdf\\'" . pdf-tools-install)
  :defer t
  :config
  (evil-set-initial-state 'pdf-view-mode 'normal))

(use-package tup-mode
  :straight (:host github :repo "ejmr/tup-mode" :branch "master"))

(use-package tex-site
  :straight auctex
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
  (setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view"))))

(use-package dtrt-indent
  :straight t)

(use-package diff-hl
  :straight t
  :config
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

(use-package deadgrep
  :straight t
  :config (evil-collection-deadgrep-setup))

(use-package magit
  :straight t
  :bind
  ("C-x g" . magit-status)

  :config
  (use-package evil-magit
    :straight t)
  (use-package with-editor
    :straight t)
  (add-hook 'with-editor-mode-hook 'evil-insert-state))

(use-package subword
  :straight t
  :config
  (global-subword-mode 1))

(use-package paredit
  :straight t
  :config
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'lisp-mode-hook 'paredit-mode)
  (add-hook 'scheme-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode))

(use-package htmlize
  :straight t)

(use-package flyspell
  :straight t
  :config
  (setq ispell-list-command "--list")
  (add-hook 'text-mode-hook 'turn-on-auto-fill)
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'git-commit-mode-hook 'flyspell-mode))

(use-package dired-hide-dotfiles
  :straight t
  :config
  (dired-hide-dotfiles-mode)
  (define-key dired-mode-map "." 'dired-hide-dotfiles-mode))

(use-package async
  :straight t
  :config
  (dired-async-mode 1))

(use-package which-key
  :straight t
  :config
  (which-key-mode))

(use-package buffer-move
  :straight t
  :config
  (global-set-key (kbd "C-S-h") 'buf-move-left)
  (global-set-key (kbd "C-S-j") 'buf-move-down)
  (global-set-key (kbd "C-S-k") 'buf-move-up)
  (global-set-key (kbd "C-S-l") 'buf-move-right))

(use-package helm
  :straight t
  :preface (require 'helm-config)
  :bind
  (("M-x" . helm-M-x)
   ("C-c f" . helm-find-files))
  :config
  (helm-mode 1))

(load-file "~/.emacs.d/sensible-defaults.el")

(sensible-defaults/use-all-settings)
;;(sensible-defaults/use-all-keybindings)
;;(sensible-defaults/bind-home-and-end-keys)
;;(sensible-defaults/backup-to-temp-directory)
(setq backup-by-copying t
      create-lockfiles nil
      backup-directory-alist '(("." . "~/.cache/emacs-backups"))
      auto-save-file-name-transforms '((".*" "~/.cache/emacs-backups/" t)))

(tool-bar-mode nil)
(set-window-scroll-bars (minibuffer-window) nil nil)
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

(global-display-line-numbers-mode)

(global-set-key
 (kbd "C-c d")
 (lambda () (interactive)
   (set-window-dedicated-p
    (selected-window)
    (if (window-dedicated-p (selected-window))
        nil t))))

(remove-hook 'before-save-hook 'delete-trailing-whitespace)

(setq gdb-many-windows t)

(setq tab-width 4)
(setq c-basic-offset 4)

;;(add-to-list 'before-make-frame-hook #'(lambda () (load-file "~/.emacs.d/init.el")))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-indent-level 4)
 '(LaTeX-item-indent -4)
 '(LaTeX-left-right-indent-level 4)
 '(TeX-brace-indent-level 4)
 '(custom-buffer-indent 4)
 '(deft-default-extension "org" t)
 '(deft-directory "~/org/roam" t)
 '(deft-recursive t t)
 '(deft-use-filter-string-for-filename t t)
 '(helm-completion-style (quote emacs))
 '(org-roam-directory "~/org/roam")
 '(package-selected-packages
   (quote
    (evil-magit which-key dired-hide-dotfiles htmlize paredit magit deadgrep diff-hl moody evil-collection org-bullets evil-org auctex pdf-tools eglot rust-mode helm-projectile projectile company solarized-theme helm use-package)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka Term" :foundry "CYEL" :slant normal :weight normal :height 157 :width normal)))))
