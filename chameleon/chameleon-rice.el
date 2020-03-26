;;; chameleon-rice.el --- Visual tweaks etc          -*- lexical-binding: t; -*-

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

;; No rice, no life.

;;; Code:

;; Don't emulate xterm please
(setq frame-resize-pixelwise t)

;; Use a sane font
(add-to-list 'default-frame-alist
             '(font . "Go Mono 10"))

;; Eternal blinking cursor
(setq-default cursor-type 'bar)
(blink-cursor-mode 1)
(setq blink-cursor-blinks 0)

;; override insert key to change cursor in overwrite mode - https://gist.github.com/fisher/04d6966491748efa5ad3
(defvar cursor-mode-status 0)
(defun my/toggle-overwrite-mode-and-change-cursor ()
  "As its name suggests."
  (interactive)
  (cond
   ((eq cursor-mode-status 0)
    (setq cursor-type 'box)
    (overwrite-mode (setq cursor-mode-status 1)))
   (t (setq cursor-type 'bar)
      (overwrite-mode (setq cursor-mode-status 0)))))
(global-set-key (kbd "<insert>")
                'my/toggle-overwrite-mode-and-change-cursor)

;; Nice frame title
(setq frame-title-format "%b %&- emacs")
(setq icon-title-format "%b %&- emacs")

;; Never ever ring the bell
(setq ring-bell-function 'ignore)

;; Start on the dashboard
(setq initial-buffer-choice '(lambda ()
                               (require 'dashboard)
                               (dashboard-insert-startupify-lists) ;; Make sure the dashboard is updated
                               (get-buffer "*dashboard*"))) ;; Open the dashboard when running emacsclient

;; Never ask me to type out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; Make mouse yanking less awkward
(setq mouse-yank-at-point t)

;; Use mouse in terminals
(add-hook 'tty-setup-hook
          (lambda ()
            (xterm-mouse-mode)))

;; stuff from ergoemacs.org
;; backup in one place. flat, no tree structure
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))
;; utf8
;; https://thraxys.wordpress.com/2016/01/13/utf-8-in-emacs-everywhere-forever/
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; https://www.emacswiki.org/emacs/SmoothScrolling
;; scroll one line at a time (less "jumpy" than defaults)

(setq mouse-wheel-progressive-speed 'nil) ;; don't accelerate scrolling

(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq scroll-step 1) ;; keyboard scroll one line at a time

;; https://www.reddit.com/r/emacs/comments/2kdztw/emacs_in_evil_mode_show_tildes_for_blank_lines/
;; Emacs-y tilde fringe
(setq-default indicate-empty-lines t)
(progn
  (define-fringe-bitmap 'tilde
    [0 0 0 113 219 142 0 0]
    nil
    nil
    'center)
  (setcdr (assq 'empty-line fringe-indicator-alist)
          'tilde))
(set-fringe-bitmap-face 'tilde 'font-lock-comment-face)

;; fundamental-mode is just about useless; default to text-mode
(setq-default major-mode 'text-mode)

;; Dired tweaks - use dired+ and pass -h to ls
(require 'dired+)
(global-dired-hide-details-mode -1)
(setq-default dired-listing-switches "-alh")
(add-hook 'dired-mode-hook
          '(lambda ()
             (dired-hide-details-mode -1) ; Please don't hide the details!
             (hl-line-mode))) ; Legibility tweak

;; fill-column-indicator: useful when hacking at work
(when (>= emacs-major-version 25)
  (use-package fill-column-indicator))

;; dump session.* files in /tmp - I really don't want these cluttering up my .emacs.d!
(eval-after-load 'x-win
  (let ((session-dir "/tmp/emacs-session/"))
    `(progn
       (make-directory ,session-dir t)
       (defun emacs-session-filename (session-id)
         "Construct a filename to save the session in based on SESSION-ID. Overrides the default to throw it into /tmp/ to get GCd by the kernel. Thanks no-littering."
         (expand-file-name session-id ,session-dir)))))

;; Anzu mode (counts isearch occurrences)
(use-package anzu :diminish anzu-mode)
(global-anzu-mode)

;; Disable text-scale-adjust - I fucking hate this function. Usually I
;; press C-x C-- by accident and then have to C-h l in order to figure
;; out what the heck I just did.
(dolist (key '("C-x C-0" "C-x C-=" "C-x C--" "C-x C-+"))
  (global-unset-key (kbd key)))

;; Save the recentf list every five minutes
(run-at-time nil (* 5 60) 'recentf-save-list)

;; Helpful
(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)))

;; Indent-tabs-mode: nil
;; Hypocritical, I know. But I don't really like Emacs' behaviour when it comes to tabs.
;; By default it *mixes* tabs and spaces for most modes (EWWWWWWWWW)
;; So tell it not to fucking do that. Byproduct is that most modes will now be
;; spaces, which is gross but inevitable. I've already kind of lost this holy war.
(setq-default indent-tabs-mode nil)

(use-package mood-line)
(mood-line-mode)
;; Can't rice on sadhu for some reason, so just leave it as is for now.

(provide 'chameleon-rice)
;;; chameleon-rice.el ends here
