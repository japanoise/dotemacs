;;; chameleon-keys.el --- My bindings                -*- lexical-binding: t; -*-

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

;; Just some basic keybindings

;;; Code:

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

(provide 'chameleon-keys)
;;; chameleon-keys.el ends here
