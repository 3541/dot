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

;(use-package evil-collection
;  :ensure t
;  :after evil
;  :config
;  (evil-collection-init))

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
  (projectile-mode))

(use-package helm-projectile
  :ensure t
  :after projectile helm
  :config
  (helm-projectile-on))

(use-package eglot
  :ensure t
  :demand t
  :bind
  (("C-c e r" . eglot-rename))
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  (add-to-list
   'eglot-server-programs
   '(rust-mode . ("/home/alex/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/bin/rust-analyzer")))
  (add-hook 'rust-mode-hook 'eglot-ensure)
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions))

(use-package yasnippet
  :ensure t
  :hook
  (rust-mode . yas-minor-mode)
  :config
  (yas-global-mode 1))

(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t))

;; (use-package moody
;;    :ensure t
;;    :config
;; ;;   (setq x-underline-at-descent-line t)
;;    (moody-replace-mode-line-buffer-identification)
;;    (moody-replace-vc-mode))

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

(use-package company-org-roam
  :ensure t
;  :after org-roam company org
  :config
  (push 'company-org-roam company-backends))

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

(use-package deft
  :ensure t
  :after org
  :bind
  ("C-c r d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/org/roam"))

(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-tools-install)
  :defer t
  :config
  (evil-set-initial-state 'pdf-view-mode 'normal))

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
  ;; (use-package evil-magit
  ;;   :ensure t)
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


(load-file "~/.emacs.d/sensible-defaults.el")

(sensible-defaults/use-all-settings)
;;(sensible-defaults/use-all-keybindings)
;;(sensible-defaults/bind-home-and-end-keys)
;;(sensible-defaults/backup-to-temp-directory)
(setq backup-by-copying t
      create-lockfiles nil
      backup-directory-alist '(("." . "~/.cache/emacs-backups"))
      auto-save-file-name-transforms '((".*" "~/.cache/emacs-backups/" t)))

(tool-bar-mode -1)
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

(setq gdb-many-windows t)

(setq tab-width 4)
(setq c-basic-offset 4)
(setq c-default-style "linux")
(setq sgml-basic-offset 4)

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
;; (let ((line (face-attribute 'mode-line :underline)))
;;   (set-face-attribute 'mode-line          nil :overline   line)
;;   (set-face-attribute 'mode-line-inactive nil :overline   line)
;;   (set-face-attribute 'mode-line-inactive nil :underline  line)
;;   (set-face-attribute 'mode-line          nil :box        nil)
;;   (set-face-attribute 'mode-line-inactive nil :box        nil)
;;   (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9"))

(set-frame-font "Iosevka Term-14" nil t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-undo-system 'undo-tree)
 '(global-display-line-numbers-mode t)
 '(helm-completion-style 'emacs)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka Term" :foundry "CYEL" :slant normal :weight normal :height 141 :width normal)))))
