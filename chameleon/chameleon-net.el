;;; chameleon-net.el --- my network packages         -*- lexical-binding: t; -*-

;; Copyright (C) 2017 japanoise aka chameleon

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

;; Packages for accessing the net
;; They're a bit hacky, but I guess that's par for the course.

;;; Code:

;; erc - irc in Emacs
(require 'erc)
(setq erc-format-nick-function 'erc-format-@nick)
(use-package erc-hl-nicks)
(defcustom znc-pass nil "Password to use to connect to znc."
  :type 'string
  :group 'erc)
(when znc-pass
  (use-package znc
    :config (setq znc-servers `(("www.seekrit.club" 1025
                                 t
                                 ((rizon "KonaKona/rizon" ,znc-pass)
                                  (freenode "KonaKona/freenode" ,znc-pass)
                                  (darenet "KonaKona/darenet" ,znc-pass)))))))

;; gopher
(use-package gopher)

(provide 'chameleon-net)
;;; chameleon-net.el ends here
