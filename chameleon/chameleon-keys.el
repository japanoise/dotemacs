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

(require 'chameleon-variables)

(global-set-key (kbd "M-]")
                'flycheck-next-error)
(global-set-key (kbd "M-[")
                'flycheck-previous-error)

(global-set-key (kbd "C-z")
                'scroll-down-command)
(global-set-key (kbd "C-M-z")
                'scroll-other-window-down) ;; Nice command from uemacs

(global-set-key (if (string-equal system-type "windows-nt")
                    (kbd "<apps>")
                  (kbd "<menu>"))
                'helm-M-x)

(global-set-key [S-mouse-2]
                'browse-url-at-mouse)

;; Use f6 as uarg, free up C-u for kill line backwards.
(global-set-key [f6]
                'universal-argument)
(define-key universal-argument-map [f6] 'universal-argument-more)
(define-key universal-argument-map "\C-u"
  nil)
(defun kill-line-backwards ()
  "Kill line backwards."
  (interactive)
  (kill-line 0))
(global-set-key (kbd "C-u")
                'kill-line-backwards)

;; Nice commands from sandy
(global-set-key (kbd "<S-home>")
                'beginning-of-buffer)
(global-set-key (kbd "<S-end>")
                'end-of-buffer)
(defun filter-replace-region ()
  "Filter the region through a shell command, replacing it."
  (interactive)
  (let ((current-prefix-arg '(0)))
    (call-interactively #'shell-command-on-region)))
(global-set-key (kbd "C-\\")
                'filter-replace-region)

;; http://whattheemacsd.com/file-defuns.el-01.html
;; Replace a useless binding (find-file-read-only) with a useful one.
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename
                  (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!"
               name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!"
                   new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name
                   (file-name-nondirectory new-name)))))))
(global-set-key (kbd "C-x C-r")
                'rename-current-buffer-file)

;; Muscle memory from helm
(require 'dired)
(define-key dired-mode-map (kbd "C-l") 'dired-up-directory)
(define-key dired-mode-map (kbd "TAB") 'dired-find-file)

;; Better use for S-<arrow> - fast window switching
(windmove-default-keybindings)

;; Since I often want to know the wordcount of a buffer
(global-set-key (kbd "C-M-=")
                'count-words)

;; wc for org mode - noonianatall's comment here http://irreal.org/blog/?p=5722
(require 'org)
(require 'dash)
(defun my/count-words-in-subtree-or-region ()
  "Counts words in an org mode subtree.  Bind this to a key in 'org-mode', e.g. C-=."
  (interactive)
  (call-interactively (if (region-active-p)
                          'count-words-region
                        'my/count-words-in-subtree)))

(defun my/count-words-in-subtree ()
  "Count words in current node and child nodes, excluding heading text."
  (interactive)
  (org-with-wide-buffer (message "%s words in subtree"
                                 (-sum (org-map-entries (lambda ()
                                                          (outline-back-to-heading)
                                                          (forward-line 1)
                                                          (while (or (looking-at org-keyword-time-regexp)
                                                                     (org-in-drawer-p))
                                                            (forward-line 1))
                                                          (count-words (point)
                                                                       (progn
                                                                         (outline-end-of-subtree)
                                                                         (point))))
                                                        nil
                                                        'tree)))))

(define-key org-mode-map (kbd "C-=") 'my/count-words-in-subtree-or-region)

;; https://www.emacswiki.org/emacs/IncrementNumber
(defun my-change-number-at-point (change)
  (let ((number (number-at-point))
        (point (point)))
    (when number
      (progn
        (forward-word)
        (search-backward (number-to-string number))
        (replace-match (number-to-string (funcall change number)))
        (goto-char point)))))
(defun my-increment-number-at-point ()
  "Increment number at point like vim's C-a."
  (interactive)
  (my-change-number-at-point '1+ ))
(defun my-decrement-number-at-point ()
  "Decrement number at point like vim's C-x."
  (interactive)
  (my-change-number-at-point '1- ))

(global-set-key (kbd "C-c a")
                'my-increment-number-at-point)
(global-set-key (kbd "C-c x")
                'my-decrement-number-at-point)

(define-key chameleon-prefix-map (kbd "a") 'my-increment-number-at-point)
(define-key chameleon-prefix-map (kbd "x") 'my-decrement-number-at-point)

(global-set-key (kbd "<kp-add>")
                'my-increment-number-at-point)
(global-set-key (kbd "<kp-subtract>")
                'my-decrement-number-at-point)

(provide 'chameleon-keys)
;;; chameleon-keys.el ends here
