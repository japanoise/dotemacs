;;; chameleon-variables.el --- variables and stuff for my init.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  chameleon aka japanoise

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

;; To be required where necessary

;;; Code:

;; ## Stuff needed for local setup
(defvar my/no-icons 'nil "Don't use all-the-icons.")
(defvar my/local-theme 'nil "Theme to use locally.")
(defun my/load-file-if-exists (file)
  "If FILE exists, load it."
  ()
  (when (file-exists-p file)
    (load-file file)))

;; ## Prefix
(progn
  (defvar chameleon-prefix-map)
  (define-prefix-command 'chameleon-prefix-map)
  (define-key chameleon-prefix-map (kbd "s") 'replace-string)
  (define-key chameleon-prefix-map (kbd "r") 'replace-regexp)
  (define-key chameleon-prefix-map (kbd "q r") 'query-replace-regexp)
  (define-key chameleon-prefix-map (kbd "q s") 'query-replace))

(provide 'chameleon-variables)
;;; chameleon-variables.el ends here
