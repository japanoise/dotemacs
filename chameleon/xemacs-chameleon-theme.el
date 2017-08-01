;;; xemacs-chameleon-theme.el --- xemacs theme

;; Copyright (C) 2005, 2006  Xavier Maillard <zedek@gnu.org>
;; Copyright (C) 2005, 2006  Brian Palmer <bpalmer@gmail.com>
;; Copyright (C) 2013 by Syohei YOSHIDA
;; Copyright (C) 2017 by japanoise/chameleon

;; Author: japanoise (aka chameleon)
;; URL: https://github.com/japanoise/dotemacs
;; Version: 0.01

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
;;
;; Port of Xemacs theme from `color-themes', with some personal-preference changes and tweaks to play nice with farmhouse-dark

;;; Code:

(deftheme xemacs-chameleon
  "xemacs theme - chameleon's changes")

(custom-theme-set-faces
 'xemacs-chameleon

 '(default ((t (:background "gray80" :foreground "black"))))
 '(cursor ((t (:background "Red3"))))
 '(border ((t (:foreground "black"))))

 '(blue ((t (:foreground "blue"))))
 '(bold ((t (:bold t))))
 '(bold-italic ((t (:italic t :bold t))))
 '(border-glyph ((t (nil))))
 '(company-preview ((t (:background "gray65" :foreground "black"))))
 '(company-preview-common ((t (:background "gray65" :foreground "white"))))
 '(company-tooltip ((t (:background "gray75" :foreground "black"))))
 '(company-tooltip-common ((t (:foreground "red"))))
 '(company-tooltip-selection ((t (:background "gray90" :foreground "black"))))
 '(custom-button-face ((t (:bold t))))
 '(custom-changed-face ((t (:background "blue" :foreground "white"))))
 '(custom-documentation-face ((t (nil))))
 '(custom-face-tag-face ((t (:underline t))))
 '(custom-group-tag-face ((t (:underline t :foreground "blue"))))
 '(custom-group-tag-face-1 ((t (:underline t :foreground "red"))))
 '(custom-invalid-face ((t (:background "red" :foreground "yellow"))))
 '(custom-link ((t (:foreground "blue" :underline t))))
 '(custom-modified-face ((t (:background "blue" :foreground "white"))))
 '(custom-rogue-face ((t (:background "black" :foreground "pink"))))
 '(custom-saved-face ((t (:underline t))))
 '(custom-set-face ((t (:background "white" :foreground "blue"))))
 '(custom-state-face ((t (:foreground "dark green"))))
 '(custom-variable-button-face ((t (:underline t :bold t))))
 '(custom-variable-tag-face ((t (:underline t :foreground "blue"))))
 '(dired-face-boring ((t (:foreground "Gray65"))))
 '(dired-face-directory ((t (:bold t))))
 '(dired-face-executable ((t (:foreground "SeaGreen"))))
 '(dired-face-flagged ((t (:background "LightSlateGray"))))
 '(dired-face-marked ((t (:background "PaleVioletRed"))))
 '(dired-face-permissions ((t (:background "grey75" :foreground "black"))))
 '(dired-face-setuid ((t (:foreground "Red"))))
 '(dired-face-socket ((t (:foreground "magenta"))))
 '(dired-face-symlink ((t (:foreground "cyan"))))
 '(font-lock-builtin-face ((t (:foreground "red3"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "blue4"))))
 '(font-lock-comment-face ((t (:foreground "blue4"))))
 '(font-lock-constant-face ((t (:foreground "red3"))))
 '(font-lock-doc-string-face ((t (:foreground "green4"))))
 '(font-lock-doc-face ((t (:foreground "medium violet red"))))
 '(font-lock-function-name-face ((t (:foreground "brown4"))))
 '(font-lock-keyword-face ((t (:foreground "red4"))))
 '(font-lock-preprocessor-face ((t (:foreground "blue3"))))
 '(font-lock-reference-face ((t (:foreground "red3"))))
 '(font-lock-string-face ((t (:foreground "green4"))))
 '(font-lock-type-face ((t (:foreground "steelblue"))))
 '(font-lock-variable-name-face ((t (:foreground "magenta4"))))
 '(font-lock-warning-face ((t (:bold t :foreground "Red"))))
 '(fringe ((t (:background "grey75" :foreground "black"))))
 '(green ((t (:foreground "green"))))
 '(gui-button-face ((t (:background "grey75" :foreground "black"))))
 '(gui-element ((t (:background "Gray80"))))
 '(hc-tab ((t (:background "gray"))))
 '(hc-trailing-whitespace ((t (:background "red"))))
 '(helm-selection ((t (:background "medium blue" :foreground "white"))))
 '(helm-buffer-file ((t (:foreground "black"))))
 '(helm-M-x-key ((t (:foreground "medium blue" :background "gray65" :underline t))))
 '(helm-buffer-directory ((t (:foreground "dark slate gray"))))
 '(helm-buffer-not-saved ((t (:foreground "midnight blue"))))
 '(helm-buffer-process ((t (:foreground "dark violet"))))
 '(helm-ff-executable ((t (:foreground "forest green"))))
 '(helm-ff-symlink ((t (:foreground "DarkOrange4"))))
 '(helm-ff-prefix ((t (:background "yellow" :foreground "black"))))
 '(helm-ff-directory ((t (:foreground "DarkRed" :background "LightGray"))))
 '(helm-ff-dotted-directory ((t (:foreground "black" :background "DimGray"))))
 '(helm-ff-dotted-symlink-directory ((t (:foreground "DarkOrange" :background "DimGray"))))
 '(helm-ff-invalid-symlink ((t (:foreground "black" :background "red"))))
 '(helm-ff-file ((t (:foreground "black"))))
 '(helm-ff-dirs ((t (:foreground "black"))))
 '(helm-source-header ((t (:family "Sans Serif" :height 1.3 :weight bold :foreground "black"))))
 '(helm-history-deleted ((t (:inherit helm-ff-invalid-symlink))))
 '(helm-history-remote ((t (:foreground "Indianred1"))))
 '(highlight ((t (:background "darkseagreen2"))))
 '(info-node ((t (:italic t :bold t))))
 '(info-xref ((t (:bold t))))
 '(isearch ((t (:background "paleturquoise" :foreground "black"))))
 '(isearch-lazy-highlight-face ((t (:foreground "white" :background "#495259"))))
 '(italic ((t (:italic t))))
 '(left-margin ((t (nil))))
 '(list-mode-item-selected ((t (:background "gray68"))))
 '(link ((t (:foreground "blue" :underline t))))
 '(minibuffer-prompt ((t (:foreground "black"))))
 '(mode-line ((t (:background "grey85" :foreground "black" :inverse-video nil :box (:line-width 1 :color "black" :style released-button)))))
 '(mode-line-inactive ((t (:background "white" :foreground "dark gray" :inverse-video nil :box (:line-width 1 :color "black" :style released-button) :slant italic))))
 '(modeline ((t (:background "Gray80"))))
 '(modeline-buffer-id ((t (:background "Gray80" :foreground "blue4"))))
 '(modeline-mousable ((t (:background "Gray80" :foreground "firebrick"))))
 '(modeline-mousable-minor-mode ((t (:background "Gray80" :foreground "green4"))))
 '(paren-blink-off ((t (:foreground "gray80"))))
 '(paren-match ((t (:background "darkseagreen2"))))
 '(paren-mismatch ((t (:background "DeepPink" :foreground "black"))))
 '(pointer ((t (nil))))
 '(primary-selection ((t (:background "gray65"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "dark red"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "blue"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "dark violet"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "dark green"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "DarkOrange4"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "maroon4"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "navy"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "DeepSkyBlue4"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "dark goldenrod"))))
 '(rainbow-delimiters-unmatched-face ((t (:background "black" :foreground "red" :weight bold))))
 '(red ((t (:foreground "red"))))
 '(region ((t (:background "gray65"))))
 '(right-margin ((t (nil))))
 '(secondary-selection ((t (:background "paleturquoise"))))
 '(sh-heredoc ((t (:inherit font-lock-doc-face))))
 '(sp-show-pair-enclosing ((t (:background "paleturquoise"))))
 '(sp-show-pair-match-face ((t (:background "paleturquoise"))))
 '(text-cursor ((t (:background "Red3" :foreground "gray80"))))
 '(toolbar ((t (:background "Gray80"))))
 '(underline ((t (:underline t))))
 '(vertical-divider ((t (:background "Gray80"))))
 '(web-mode-doctype-face ((t (:background "black" :foreground "Grey"))))
 '(web-mode-html-attr-name-face ((t (:foreground "dark red"))))
 '(web-mode-html-tag-face ((t (:foreground "dark blue"))))
 '(widget-button-face ((t (:bold t))))
 '(widget-button-pressed-face ((t (:foreground "red"))))
 '(widget-documentation-face ((t (:foreground "dark green"))))
 '(widget-field-face ((t (:background "gray85" :foreground "black"))))
 '(widget-inactive-face ((t (:foreground "dim gray"))))
 '(yellow ((t (:foreground "yellow"))))
 '(zmacs-region ((t (:background "gray65")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'xemacs-chameleon)

;;; xemacs-chameleon-theme.el ends here
