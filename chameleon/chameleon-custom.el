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
 '(custom-safe-themes
   (quote
    ("cdd26fa6a8c6706c9009db659d2dffd7f4b0350f9cc94e5df657fa295fffec71" "e8825f26af32403c5ad8bc983f8610a4a4786eb55e3a363fa9acb48e0677fe7e" "432c18be21dc506512cb44bf93ab7d10741ad64c979da53299df03c1a0478769" "3629b62a41f2e5f84006ff14a2247e679745896b5eaa1d5bcfbc904a3441b0cd" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "ecfb30f9d1748ab722d4b7d25d28c130d774146cc5aec3fd7cbe0827cfdcd6a9" "72c530c9c8f3561b5ab3bf5cda948cd917de23f48d9825b7a781fe1c0d737f2f" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
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
    (spacemacs-theme go-rename rainbow-delimiters memoize all-the-icons emojify monokai-theme diff-hl helm-projectile neotree markdown-mode smartparens smartparens-config web-mode smart-tabs-mode smart-tabs highlight-chars flycheck which-key undo-tree delight projectile dashboard page-break-lines whitespace-cleanup-mode org-bullets helm)))
 '(projectile-mode t nil (projectile))
 '(recentf-mode t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(sml/mode-width (quote full))
 '(sml/name-width 50)
 '(sml/no-confirm-load-theme t)
 '(sml/pos-minor-modes-separator "")
 '(sml/theme (quote light))
 '(tool-bar-mode nil)
 '(which-key-echo-keystrokes 0.1)
 '(which-key-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(minibuffer-prompt ((t (:inherit default)))))

(provide 'chameleon-custom)
;;; chameleon-custom.el ends here
