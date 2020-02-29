;; heavily based on https://github.com/hrs/dotfiles/blob/master/emacs/.emacs.d/configuration.org

(require 'package)
(load-file "~/.emacs.d/sensible-defaults.el")

(sensible-defaults/use-all-settings)
;;(sensible-defaults/use-all-keybindings)
(sensible-defaults/bind-home-and-end-keys)
(sensible-defaults/backup-to-temp-directory)

(tool-bar-mode nil)
(set-window-scroll-bars (minibuffer-window) nil nil)

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

(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
;;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode t)
  (define-key evil-motion-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-motion-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-motion-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-motion-state-map (kbd "C-l") 'evil-window-right))

(use-package evil-collection
  :ensure t
  :after evil)

(use-package helm
  :ensure t)

(use-package solarized-theme
  :ensure t
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
  :ensure t
  :config
  (setq x-underline-at-descent-line t)
;;  (moody-replace-mode-line-buffer-identification)
;;  (moody-replace-vc-mode)
  )

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.2)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package projectile
  :ensure t
  :after helm
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
  (projectile-mode +1))

(use-package helm-projectile
  :ensure t
  :after projectile
  :config
  (helm-projectile-on))

(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t))

(use-package eglot
  :ensure t
  :config
  (add-hook 'rust-mode-hook 'eglot-ensure))

(use-package org
  :ensure t
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
  (setq org-directory "~/org")
  (setq org-index-file "~/org/index.org")
  (setq org-agenda-files (list org-index-file))
  (setq org-agenda-span 14)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (C . t)
     (python . t)
     (ruby . t))))

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

(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-tools-install)
  :defer t)

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
  (setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view"))))

(use-package dtrt-indent
  :ensure t)

(use-package diff-hl
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

(use-package deadgrep
  :ensure t
  :config (evil-collection-deadgrep-setup))

(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status)

  :config
  (use-package evil-magit
    :ensure t)
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
 '(package-selected-packages
   (quote
    (which-key dired-hide-dotfiles htmlize paredit magit deadgrep diff-hl moody evil-collection org-bullets evil-org auctex pdf-tools eglot rust-mode helm-projectile projectile company solarized-theme helm use-package)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka Term" :foundry "CYEL" :slant normal :weight normal :height 157 :width normal)))))
