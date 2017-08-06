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
 '(ansi-color-names-vector
   ["#242728" "#ff0066" "#63de5d" "#E6DB74" "#06d8ff" "#ff8eff" "#53f2dc" "#f8fbfc"])
 '(async-bytecomp-package-mode t)
 '(battery-mode-line-format " %p" t)
 '(blink-cursor-blinks 0)
 '(blink-cursor-mode t)
 '(column-number-mode t)
 '(company-idle-delay 0.2)
 '(compilation-message-face (quote default))
 '(custom-safe-themes t)
 '(erc-track-position-in-mode-line t t)
 '(fci-rule-color "#424748")
 '(frame-resize-pixelwise t)
 '(global-company-mode t)
 '(global-diff-hl-mode t)
 '(global-flycheck-mode t)
 '(global-page-break-lines-mode t)
 '(global-undo-tree-mode t)
 '(global-whitespace-cleanup-mode t)
 '(helm-buffer-max-length nil)
 '(helm-mode t)
 '(highlight-changes-colors (quote ("#ff8eff" "#ab7eff")))
 '(highlight-tail-colors
   (quote
    (("#424748" . 0)
     ("#63de5d" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#424748" . 100))))
 '(inhibit-startup-screen t)
 '(magit-diff-use-overlays nil)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (doom-themes rainbow-mode go-rename rainbow-delimiters memoize all-the-icons emojify monokai-theme diff-hl helm-projectile neotree markdown-mode smartparens smartparens-config web-mode smart-tabs-mode smart-tabs highlight-chars flycheck which-key undo-tree delight projectile dashboard page-break-lines whitespace-cleanup-mode org-bullets helm)))
 '(pos-tip-background-color "#E6DB74")
 '(pos-tip-foreground-color "#242728")
 '(projectile-mode t nil (projectile))
 '(recentf-mode t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(sml/mode-width (quote full))
 '(sml/name-width 50)
 '(sml/no-confirm-load-theme t)
 '(sml/pos-minor-modes-separator "")
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#ff0066")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#63de5d")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#53f2dc")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#06d8ff"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#242728" "#424748" "#F70057" "#ff0066" "#86C30D" "#63de5d" "#BEB244" "#E6DB74" "#40CAE4" "#06d8ff" "#FF61FF" "#ff8eff" "#00b2ac" "#53f2dc" "#f8fbfc" "#ffffff"))
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

(provide 'chameleon-custom)
;;; chameleon-custom.el ends here
