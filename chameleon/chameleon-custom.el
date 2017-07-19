;;; chameleon-custom --- my customizations
;;; Commentary:
;;; This is where customize should put all my customizations.
;;; The faces are intended to work nicely with the Xemacs theme on top of tango.
;;; Code:
(setq custom-file "~/.emacs.d/chameleon/chameleon-custom.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-blinks 0)
 '(company-idle-delay 0.2)
 '(custom-safe-themes
   (quote
    ("72c530c9c8f3561b5ab3bf5cda948cd917de23f48d9825b7a781fe1c0d737f2f" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(frame-resize-pixelwise t)
 '(helm-buffer-max-length nil)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (monokai-theme diff-hl helm-projectile neotree markdown-mode smartparens smartparens-config web-mode smart-tabs-mode smart-tabs highlight-chars flycheck which-key undo-tree delight projectile dashboard page-break-lines whitespace-cleanup-mode org-bullets helm)))
 '(sml/mode-width (quote full))
 '(sml/name-width 50)
 '(sml/no-confirm-load-theme t)
 '(sml/pos-minor-modes-separator "")
 '(sml/theme (quote light))
 '(which-key-echo-keystrokes 0.1))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "blue4" :slant normal))))
 '(font-lock-doc-face ((t (:foreground "medium violet red"))))
 '(hc-tab ((t (:background "gray"))))
 '(hc-trailing-whitespace ((t (:background "red"))))
 '(helm-M-x-key ((t (:foreground "medium blue" :underline t))))
 '(helm-buffer-directory ((t (:foreground "dark slate gray"))))
 '(helm-buffer-not-saved ((t (:foreground "midnight blue"))))
 '(helm-buffer-process ((t (:foreground "dark violet"))))
 '(helm-ff-executable ((t (:foreground "forest green"))))
 '(helm-ff-symlink ((t (:foreground "DarkOrange4"))))
 '(isearch ((t (:background "paleturquoise" :foreground "black"))))
 '(sh-heredoc ((t (:inherit font-lock-doc-face))))
 '(web-mode-doctype-face ((t (:background "black" :foreground "Grey"))))
 '(web-mode-html-attr-name-face ((t (:foreground "dark red"))))
 '(web-mode-html-tag-face ((t (:foreground "dark blue")))))

(provide 'chameleon-custom)
;;; chameleon-custom.el ends here
