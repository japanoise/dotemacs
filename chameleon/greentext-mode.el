;;; greentext-mode --- Major mode for Greentext stories
;;; Commentary:
;;; Code:
(defconst greentext-font-lock-keywords
  (list
   '("^>.*" . font-lock-string-face)))
(define-derived-mode greentext-mode
  text-mode
  "Greentext"
  "Major mode for editing greentext stories."
  (setq font-lock-defaults '(greentext-font-lock-keywords)))
(fset 'insert-meme-arrow
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([5 return 62] 0 "%d")) arg)))
(define-key greentext-mode-map (kbd "<S-return>") 'insert-meme-arrow)
(define-key greentext-mode-map (kbd "<M-return>") 'insert-meme-arrow)
(define-key greentext-mode-map (kbd "<C-return>") 'insert-meme-arrow)
(provide 'greentext-mode)
;;; greentext-mode ends here
