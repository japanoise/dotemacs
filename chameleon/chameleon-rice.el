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

(provide 'chameleon-rice)
;;; chameleon-rice.el ends here
