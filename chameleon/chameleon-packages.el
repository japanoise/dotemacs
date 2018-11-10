;;; chameleon-packages.el --- Install my packages    -*- lexical-binding: t; -*-
;;; Commentary:
;; Copyright (C) 2017
;;; Code:

(eval-when-compile (add-to-list 'load-path "~/.emacs.d/chameleon")
                   (add-to-list 'load-path "~/.emacs.d/vendor"))

(require 'chameleon-variables)

;; ## Enable packages
(eval-when-compile (require 'use-package)
                   (setq use-package-always-ensure t)) ;; Make use-package auto-installs everything
(use-package diminish) ;; While this is bundled with Emacs 25, it isn't with 24.
(require 'highlight-chars) ;; Vendored
(add-hook 'prog-mode-hook 'hc-highlight-trailing-whitespace)
(use-package color-theme-modern)
(use-package smart-tabs-mode)
(use-package whitespace-cleanup-mode) ;; Since my .vimrc used to do the same thing
(use-package delight) ;; Shorten some minor modes
(use-package emojify) ;; :joy: :ok_hand:

;; Rainbow Delimiters
(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Smartparens
(use-package smartparens
  :diminish smartparens-mode
  :bind (("C-M-f" . sp-forward-sexp)
         ("C-M-b" . sp-backward-sexp)))
(require 'smartparens-config)

;; Flycheck and Flyspell
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

;; Company
(use-package company
  :diminish company-mode
  :config (add-hook 'after-init-hook 'global-company-mode))

;; Magit, with some bindings
(use-package magit
  :bind (("C-x g" . magit-status) :map chameleon-prefix-map
         ("g s" . magit-status)))
(use-package diff-hl)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; Replace ^L with a horizontal rule
(use-package page-break-lines :diminish page-break-lines-mode)

;; Remove the mode name for projectile-mode, but show the project name.
(use-package projectile
  :delight '(:eval (concat " "
                           (projectile-project-name))))

;; Non-confusing undo
(use-package undo-tree
  :diminish undo-tree-mode
  :bind (("C-?" . undo-tree-redo)))

;; Helm... The big boy :^)
(use-package helm
  :diminish helm-mode
  :ensure t
  :init (progn
          (require 'helm-config)
          (helm-mode 1)):bind
          (("M-x" . helm-M-x) ;; Please use helm everywhere xoxoxo
           ("C-x b" . helm-mini)
           ("C-x C-b" . helm-buffers-list)
           ("C-x C-f" . helm-find-files)
           ("M-y" . helm-show-kill-ring)
           :map helm-map
           ("<tab>" . helm-execute-persistent-action)
           ("C-i" . helm-execute-persistent-action)
           ("C-z" . helm-select-action))
          :config (progn
                    (setq helm-split-window-in-side-p t)
                    (require 'helm-files)
                    (define-key helm-find-files-map (kbd "C-x 4 C-f") #'helm-ff-run-switch-other-window)))
(use-package helm-projectile
  :init (require 'helm-projectile)(helm-projectile-on))

;; Spacemacs-style dashboard
(use-package dashboard
  :ensure t
  :diminish dashboard-mode
  :bind (:map dashboard-mode-map
              ("<down-mouse-1>" . nil)
              ("<mouse-1>" . widget-button-click)
              ("<mouse-2>" . widget-button-click)):config
              (setq dashboard-banner-logo-title "EmacsOS - Ready.")
              (setq dashboard-startup-banner 'official)
              (setq dashboard-items '((recents . 10)
                                      (projects . 10)
                                      (agenda . 5)))
              (dashboard-setup-startup-hook)
              :init (require 'dashboard)(dashboard-insert-startupify-lists))

;; Which-key - spacemacs' nice little prefix popup
(use-package which-key :diminish which-key-mode)
(setq which-key-echo-keystrokes 0.1)

;; Neotree
(if (not my/no-icons)
    (use-package all-the-icons)
  '())
(use-package neotree
  :bind (([f8] . neotree-toggle) :map chameleon-prefix-map
         ("t s" . neotree)
         ("t t" . neotree-toggle)
         ("t p" . neotree-projectile-action)):init
         (setq neo-smart-open t)
         (setq projectile-switch-project-action 'neotree-projectile-action)
         (if (and (package-installed-p 'all-the-icons)
                  (not my/no-icons))
             (setq neo-theme 'icons)
           (setq neo-theme 'classic)))

;; Rainbow mode
(use-package rainbow-mode)
(provide 'chameleon-packages)

;; Stuff for org-mode
(use-package org-bullets)
(use-package htmlize)
;; org-mode encryption - http://orgmode.org/worg/org-tutorials/encrypting-files.html
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
(setq org-crypt-key nil)

;;; chameleon-packages.el ends here
