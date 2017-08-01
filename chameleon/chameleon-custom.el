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
 '(async-bytecomp-package-mode t)
 '(battery-mode-line-format " %p" t)
 '(blink-cursor-blinks 0)
 '(blink-cursor-mode t)
 '(column-number-mode t)
 '(company-idle-delay 0.2)
 '(custom-safe-themes t)
 '(darkokai-high-contrast-mode-line t)
 '(erc-track-position-in-mode-line t t)
 '(frame-resize-pixelwise t)
 '(global-company-mode t)
 '(global-diff-hl-mode t)
 '(global-flycheck-mode t)
 '(global-page-break-lines-mode t)
 '(global-undo-tree-mode t)
 '(global-whitespace-cleanup-mode t)
 '(helm-buffer-max-length nil)
 '(helm-mode t)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (rainbow-mode darkokai-theme monokai spacemacs-theme go-rename rainbow-delimiters memoize all-the-icons emojify monokai-theme diff-hl helm-projectile neotree markdown-mode smartparens smartparens-config web-mode smart-tabs-mode smart-tabs highlight-chars flycheck which-key undo-tree delight projectile dashboard page-break-lines whitespace-cleanup-mode org-bullets helm)))
 '(projectile-mode t nil (projectile))
 '(recentf-mode t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(sml/mode-width (quote full))
 '(sml/name-width 50)
 '(sml/no-confirm-load-theme t)
 '(sml/pos-minor-modes-separator "")
 '(tool-bar-mode nil)
 '(when
      (or
       (not
        (boundp
         (quote ansi-term-color-vector)))
       (not
        (facep
         (aref ansi-term-color-vector 0)))))
 '(which-key-echo-keystrokes 0.1)
 '(which-key-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-theme-set-faces
 'darkokai
 '(font-lock-comment-face ((t (:foreground "#1cf" :background "#242736"))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
 '(mode-line ((t (:box (:line-width 1 :color "grey" :style released-button)))))
 '(mode-line-inactive ((t (:box (:line-width 1 :color "black" :style released-button))))))

(provide 'chameleon-custom)
;;; chameleon-custom.el ends here
