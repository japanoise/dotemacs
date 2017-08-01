;;; init --- my init file
;;; Commentary:
;;; Just another Emacs hacker,
;;; Code:

;; ## Local setup
(defvar my/no-icons 'nil "Don't use all-the-icons.")
(if (file-exists-p "~/.emacs.d/chameleon-local.el")
    (load-file "~/.emacs.d/chameleon-local.el")
  '())

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

;; ## My prefix - bound to <f5> by default
(progn
  (defvar chameleon-prefix-map)
  (define-prefix-command 'chameleon-prefix-map)
  (define-key chameleon-prefix-map (kbd "s") 'replace-string)
  (define-key chameleon-prefix-map (kbd "r") 'replace-regexp)
  (define-key chameleon-prefix-map (kbd "q r") 'query-replace-regexp)
  (define-key chameleon-prefix-map (kbd "q s") 'query-replace))
(global-set-key (kbd "<f5>") 'chameleon-prefix-map)

;; ## Enable packages
(eval-when-compile ;; Make use-package auto-install everything
  (require 'use-package)
  (setq use-package-always-ensure t))
(require 'diminish) ;; Bundled with emacs, no need to u-p it
(use-package highlight-chars
  :init (add-hook 'prog-mode-hook 'hc-highlight-trailing-whitespace))
(use-package org-bullets)
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

;; ### Programming language modes
;; Go mode. Dependencies:
;; - github.com/nsf/gocode
;; - github.com/godoctor/godoctor
;; - golang.org/x/tools/cmd/goimports
;; - github.com/rogpeppe/godef
;; - golang.org/x/tools/cmd/gorename
(use-package go-mode
  :config (use-package godoctor)
  :bind(("C-c C-r" . go-remove-unused-imports)))
(use-package company-go)
(defun my-go-mode-hook ()
  "Hook for go mode.  Use goimports, godef, setup compile command."
  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark)
  (set (make-local-variable 'company-backends) '(company-go)))
(add-hook 'go-mode-hook 'my-go-mode-hook)
(use-package go-eldoc
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))
(use-package go-rename)

;; Web mode
(use-package web-mode
  :mode ("\\.html?\\'" . web-mode))
;; Smartparens for web-mode
(defun my-web-mode-hook ()
  "Hook for web-mode with smartparens."
  (setq web-mode-enable-auto-pairing nil))
(add-hook 'web-mode-hook  'my-web-mode-hook)
(defun sp-web-mode-is-code-context (id action context)
  "Determines whether we're in code context in web-mode.  Takes args ID ACTION CONTEXT."
  (and (eq action 'insert)
       (not (or (get-text-property (point) 'part-side)
                (get-text-property (point) 'block-side)))))
(sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))

;; Markdown mode
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; ### End of programming modes

;; Magit, with some bindings
(use-package magit
  :bind(:map chameleon-prefix-map
        ("g s" . magit-status)))
(use-package diff-hl)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

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
       ("C-x b" . helm-mini)
       ("C-x C-b" . helm-buffers-list)
       ("C-x C-f" . helm-find-files)
       ("M-y" . helm-show-kill-ring)
       :map helm-map
       ("<tab>" . helm-execute-persistent-action)
       ("C-i" . helm-execute-persistent-action)
       ("C-z" . helm-select-action))
  :config
  (setq helm-split-window-in-side-p t) ;; Always popup at the bottom, don't take over other window
  (require 'helm-files)
  (define-key helm-find-files-map ;; I mean it. Use helm *everywhere*
    (kbd "C-x 4 C-f")
    #'helm-ff-run-switch-other-window))
(use-package helm-projectile
  :init (require 'helm-projectile)
  (helm-projectile-on))

;; Spacemacs-style dashboard
(use-package dashboard
  :ensure t
  :diminish dashboard-mode
  :bind (
         :map dashboard-mode-map
         ("<down-mouse-1>" . nil)
         ("<mouse-1>" . widget-button-click)
         ("<mouse-2>" . widget-button-click))
  :config
  (setq dashboard-banner-logo-title "EmacsOS - Ready.")
  (setq dashboard-startup-banner 'official)
  (setq dashboard-items '((recents  . 10)
                          (projects . 10)
                          (agenda . 5)))
  (dashboard-setup-startup-hook)
  :init
  (require 'dashboard)
  (dashboard-insert-startupify-lists))

;; Which-key - spacemacs' nice little prefix popup
(use-package which-key
  :diminish which-key-mode)

;; Neotree
(if (not my/no-icons) (use-package all-the-icons) '())
(use-package neotree
  :bind(
        ([f8] . neotree-toggle)
        :map chameleon-prefix-map
        ("t s" . neotree)
        ("t t" . neotree-toggle)
        ("t p" . neotree-projectile-action))
  :init (setq neo-smart-open t)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (if (and (package-installed-p 'all-the-icons) (not my/no-icons))
      (setq neo-theme 'icons) (setq neo-theme 'classic)))

;; ## Major mode hooks
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-hook 'prog-mode-hook #'smartparens-mode)
(add-hook 'emacs-lisp-mode-hook (lambda () (setq indent-tabs-mode nil)))

;; ## Minor Modes
(require 'paren)
(setq show-paren-delay 0)
(show-paren-mode 1)

(recentf-mode 1)
(setq recentf-max-menu-items 25)

(global-whitespace-cleanup-mode)
(diminish 'whitespace-cleanup-mode)

(global-diff-hl-mode 1)
(global-page-break-lines-mode 1)
(global-undo-tree-mode 1)
(column-number-mode t)
(which-key-mode)

;; ## Misc. Customization
;; Keybindings
(global-set-key (kbd "M-]") 'flycheck-next-error)
(global-set-key (kbd "M-[") 'flycheck-previous-error)
(global-set-key (kbd "C-z") 'scroll-down-command)
(global-set-key (kbd "C-M-z") 'scroll-other-window-down) ;; Nice command from uemacs
(global-set-key (if (string-equal system-type "windows-nt") (kbd "<apps>") (kbd "<menu>")) 'helm-M-x)
(global-set-key [S-mouse-2] 'browse-url-at-mouse)
;; Use f6 as uarg, free up C-u for kill line backwards.
(global-set-key [f6] 'universal-argument)
(define-key universal-argument-map [f6] 'universal-argument-more)
(define-key universal-argument-map "\C-u" nil)
(defun kill-line-backwards () "Kill line backwards." (interactive) (kill-line 0))
(global-set-key (kbd "C-u") 'kill-line-backwards)
;; Nice commands from sandy
(global-set-key (kbd "<S-home>") 'beginning-of-buffer)
(global-set-key (kbd "<S-end>") 'end-of-buffer)
(defun filter-replace-region ()
  "Filter the region through a shell command, replacing it."
  (interactive)
  (let ((current-prefix-arg '(0)))
    (call-interactively #'shell-command-on-region)))
(global-set-key (kbd "C-\\") 'filter-replace-region)

;; Rice
(add-to-list 'default-frame-alist '(font . "Go Mono 10"))
(setq-default cursor-type 'bar)
(blink-cursor-mode 1)
(setq frame-title-format "%b - emacs")
(setq icon-title-format "%b - emacs")
(setq ring-bell-function 'ignore)
(setq initial-buffer-choice '(lambda ()
                               (require 'dashboard)
                               (dashboard-insert-startupify-lists) ;; Make sure the dashboard is updated
                               (get-buffer "*dashboard*"))) ;; Open the dashboard when running emacsclient
(defalias 'yes-or-no-p 'y-or-n-p) ;; Never ask me to type out 'yes' or 'no'
(setq mouse-yank-at-point t) ;; Oh my god yes
(add-hook 'tty-setup-hook (lambda () (xterm-mouse-mode)))

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

;; https://www.reddit.com/r/emacs/comments/2kdztw/emacs_in_evil_mode_show_tildes_for_blank_lines/
;; Emacs-y tilde fringe
(setq-default indicate-empty-lines t)
(progn
  (define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
  (setcdr (assq 'empty-line fringe-indicator-alist) 'tilde))
(set-fringe-bitmap-face 'tilde 'font-lock-comment-face)

;; Color themes; switch between light and dark theme easily
(setq inhibit-x-resources t) ;; Never load settings from .Xresources

(load-file "~/.emacs.d/chameleon/xemacs-chameleon-theme.el")
(use-package darkokai-theme
  :init (setq darkokai-mode-line-padding 1))
(defvar chameleon/themes '(darkokai xemacs-chameleon) "Themes to rotate through.")
(mapc (lambda (theme) (load-theme theme t t)) chameleon/themes)
(defun chameleon/rotate-themes () "Switch to the next theme in chameleon/themes."
       (interactive)
       (require 'dash)
       (mapc (lambda (theme)
               (when (member theme chameleon/themes)
                 (disable-theme theme)))
             custom-enabled-themes)
       (smart-mode-line-enable)
       (enable-theme (car chameleon/themes))
       (setq chameleon/themes (-rotate (- (length chameleon/themes) 1) chameleon/themes))
       (redraw-display))
(global-set-key (kbd "C-c C-t") 'chameleon/rotate-themes)

;; HACK: darkokai enables itself, but in such a way that we can't see it.
;; Enable it explicitly, then disable it again.
(enable-theme 'darkokai)
(disable-theme 'darkokai)

;; http://pages.sachachua.com/.emacs.d/Sacha.html
;; Save lots of history
(require 'savehist)
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

;; Enabled commands
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; ## Custom-set
(load-file "~/.emacs.d/chameleon/chameleon-custom.el") ;; Boot this to another file.
(enable-theme 'xemacs-chameleon)
(use-package smart-mode-line) ;; Make the modeline suck less; this needs to come after customise
(setq sml/theme 'respectful)
(sml/setup)

(provide 'init)
;;; init.el ends here
