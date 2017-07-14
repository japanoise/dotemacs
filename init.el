;; ## Custom-set
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-blinks 0)
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (which-key undo-tree delight projectile dashboard page-break-lines smart-mode-line whitespace-cleanup-mode org-bullets helm)))
 '(which-key-echo-keystrokes 0.1))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ## Set up package lists & use-package
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; ## Enable packages
(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))
(require 'diminish)
(use-package org-bullets)
(use-package whitespace-cleanup-mode)
(sml/setup) ; smart-mode-line

;; Magit, with some bindings
(use-package magit
  :bind(("M-n g s" . magit-status)))

;; Replace ^L with a horizontal rule
(use-package page-break-lines
  :diminish page-break-lines-mode)

;; Remove the mode name for projectile-mode, but show the project name.
(use-package projectile
  :delight '(:eval (concat " " (projectile-project-name))))

;; Non-confusing undo
(use-package undo-tree
  :diminish undo-tree-mode
  :bind(("C-?" . undo-tree-redo)))

;; Helm... The big boy :^)
(use-package helm
  :diminish helm-mode ;; I don't care that helm's active!
  :ensure t
  :init
  (progn
    (require 'helm-config)
    (helm-mode 1))
  :bind(("M-x" . helm-M-x) ;; Please use helm everywhere xoxoxo
	("C-x b" . helm-buffers-list)
	("C-x C-b" . helm-buffers-list)
	("C-x C-f" . helm-find-files))
  :config
  (require 'helm-files)
  (define-key helm-find-files-map ;; I mean it. Use helm *everywhere*
    (kbd "C-x 4 C-f")
    #'helm-ff-run-switch-other-window))

;; Spacemacs-style dashboard
(use-package dashboard
  :ensure t
  :diminish dashboard-mode
  :config
  (setq dashboard-banner-logo-title "EmacsOS - Ready.")
  (setq dashboard-startup-banner 'official)
  (setq dashboard-items '((recents  . 10)
			  (projects . 10)
			  (agenda . 5)))
  (dashboard-setup-startup-hook))

;; Which-key - spacemacs' nice little prefix popup
(use-package which-key)

;; ## Major mode hooks
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; ## Minor Modes
(setq show-paren-delay 0)
(show-paren-mode 1)

(recentf-mode 1)
(setq recentf-max-menu-items 25)

(global-whitespace-cleanup-mode)
(diminish 'whitespace-cleanup-mode)

(global-page-break-lines-mode 1)
(global-undo-tree-mode 1)
(column-number-mode t)
(which-key-mode)

;; ## Misc. Customization
(setq-default cursor-type 'bar)
(setq frame-title-format "%b - emacs")
(setq icon-title-format "%b - emacs")
(setq ring-bell-function 'ignore)

;; Remove visual clutter
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)

;; stuff from ergoemacs.org
;; backup in one place. flat, no tree structure
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))
;; utf8
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; https://www.emacswiki.org/emacs/SmoothScrolling
;; scroll one line at a time (less "jumpy" than defaults)

(setq mouse-wheel-progressive-speed 'nil) ;; don't accelerate scrolling

(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq scroll-step 1) ;; keyboard scroll one line at a time
