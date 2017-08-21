;;; chameleon-prog.el --- Programming language modes  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 chameleon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Programming language modes for my init file

;;; Code:

(defun buffer-mode (&optional buffer-or-name)
  "Return the major mode associated with a buffer.  If BUFFER-OR-NAME is nil return current buffer's mode."
  (buffer-local-value 'major-mode
                      (if buffer-or-name
                          (get-buffer buffer-or-name)
                        (current-buffer))))

;; Generic-x; various random major modes
;; Do it first so anything defined here can be overwritten by subsequent prog-modes
(require 'generic-x)
(add-to-list 'auto-mode-alist
             '("\\.gitignore$" . hosts-generic-mode)) ;; Highlight comments for .gitignore files

;; Elisp
(use-package srefactor)
(require 'srefactor-lisp)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (add-hook 'before-save-hook
                      (lambda ()
                        (when (eq (buffer-mode) 'emacs-lisp-mode)
                          (srefactor-lisp-format-buffer)))
                      nil
                      'local)
            (setq indent-tabs-mode nil)))

;; Go mode. Dependencies:
;; - github.com/nsf/gocode
;; - github.com/godoctor/godoctor
;; - golang.org/x/tools/cmd/goimports
;; - github.com/rogpeppe/godef
;; - golang.org/x/tools/cmd/gorename
(use-package go-mode
  :config (use-package godoctor):bind
  (("C-c C-r" . go-remove-unused-imports)))
(use-package company-go)
(defun my-go-mode-hook ()
  "Hook for go mode.  Use goimports, godef, setup compile command."
  ;; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ;; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  ;; Godef jump key binding
  (local-set-key (kbd "M-.")
                 'godef-jump)
  (local-set-key (kbd "M-*")
                 'pop-tag-mark)
  ;; More known behaviour from IntelliJ IDEA
  (local-set-key (kbd "<C-down-mouse-1>")
                 'mouse-set-point)
  (local-set-key (kbd "<C-mouse-1>")
                 'godef-jump)
  (set (make-local-variable 'company-backends)
       '(company-go)))
(add-hook 'go-mode-hook 'my-go-mode-hook)
(use-package go-eldoc
  :init (add-hook 'go-mode-hook 'go-eldoc-setup))
(use-package go-rename
  :bind (("<S-f6>" . go-rename)))

;; Web mode
(use-package web-mode
  :mode ("\\.html?\\'" . web-mode))
;; Smartparens for web-mode
(defun my-web-mode-hook ()
  "Hook for web-mode with smartparens."
  (setq web-mode-enable-auto-pairing nil))
(add-hook 'web-mode-hook 'my-web-mode-hook)
(defun sp-web-mode-is-code-context (id action context)
  "Determines whether we're in code context in web-mode.  Takes args ID ACTION CONTEXT."
  (and (eq action 'insert)
       (not (or (get-text-property (point)
                                   'part-side)
                (get-text-property (point)
                                   'block-side)))))
(sp-local-pair 'web-mode
               "<"
               nil
               :when '(sp-web-mode-is-code-context))

;; Markdown mode
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode):mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Haskell mode.
(use-package haskell-mode)
(add-hook 'haskell-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 (append '((company-capf company-dabbrev-code))
                         company-backends))))
(require 'haskell)
(setq haskell-completions-complete-operators
      nil)

;; Dockerfile mode
(use-package dockerfile-mode)

;; SLIME
(defvar my/path-to-sbcl (executable-find "sbcl")
  "Path to SBCL executable.")
(if my/path-to-sbcl
    (progn
      (use-package slime)
      (require 'slime)
      (require 'slime-autoloads)
      (add-to-list 'slime-contribs 'slime-fancy)
      (setq inferior-lisp-program my/path-to-sbcl)
      (add-hook 'slime-repl-mode-hook #'smartparens-strict-mode))
  (message "Unable to find SBCL in PATH; please install it if you want to hack on CL."))

(provide 'chameleon-prog)
;;; chameleon-prog.el ends here
