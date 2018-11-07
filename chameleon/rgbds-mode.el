;;; rgbds-mode --- Major mode for Gameboy assembly
;;; Commentary:
;;; Based on https://www.cemetech.net/forum/viewtopic.php?t=6413&start=0
;;; Code:
(require 'mwim)

(defconst rgbds-font-lock-keywords-1
  (list
   '(";.*" . font-lock-comment-face)
   '("^\\*.*" . font-lock-comment-face)
   '("\\<\\(ADC\\|ADD\\|AND\\|BIT\\|CALL\\|CCF\\|CP\\|CPD\\|CPDR\\|CPIR\\|CPI\\|CPL\\|DAA\\|DEC\\|DI\\|DJNZ\\|EI\\|EX\\|EXX\\|HALT\\|IM\\|IN\\|INC\\|IND\\|INDR\\|INI\\|INIR\\|JP\\|JR\\|LD\\|LDD\\|LDDR\\|LDI\\|LDIR\\|NEG\\|NOP\\|MLT\\|OR\\|OTDM\\|OTDMR\\|OTDR\\|OTIM\\|OTIMR\\|OTIR\\|OUT\\|OUTD\\|OUTI\\|POP\\|PUSH\\|RES\\|RET\\|RETI\\|RETN\\|RL\\|RLA\\|RLC\\|RLCA\\|RLD\\|RR\\|RRA\\|RRC\\|RRCA\\|RRD\\|RST\\|SBC\\|SCF\\|SET\\|SLA\\|SLP\\|SRA\\|SRL\\|SUB\\|TST\\|TSTIO\\|XOR\\|adc\\|add\\|and\\|bit\\|call\\|ccf\\|cp\\|cpd\\|cpdr\\|cpir\\|cpi\\|cpl\\|daa\\|dec\\|di\\|djnz\\|ei\\|ex\\|exx\\|halt\\|im\\|in\\|inc\\|ind\\|indr\\|ini\\|inir\\|jp\\|jr\\|ld\\|ldd\\|lddr\\|ldi\\|ldir\\|neg\\|nop\\|mlt\\|or\\|otdm\\|otdmr\\|otdr\\|otim\\|otimr\\|otir\\|out\\|outd\\|outi\\|pop\\|push\\|res\\|ret\\|reti\\|retn\\|rl\\|rla\\|rlc\\|rlca\\|rld\\|rr\\|rra\\|rrc\\|rrca\\|rrd\\|rst\\|sbc\\|scf\\|set\\|sla\\|slp\\|sra\\|srl\\|sub\\|tst\\|tstio\\|xor\\|A\\|B\\|C\\|D\\|E\\|H\\|L\\|AF\\|BC\\|DE\\|HL\\|IX\\|IY\\|SP\\|PC\\|a\\|b\\|c\\|d\\|e\\|h\\|l\\|af\\|bc\\|de\\|hl\\|ix\\|iy\\|sp\\|pc\\|\\|NC\\|C\\|M\\|nc\\|c\\|m\\|Z\\|z\\|NZ\\|nz\\|\\|[Pp]\\([Oo]*\\|[Ee]*\\)\\|BCALL\\|bcall\\)\\>" . font-lock-builtin-face)
   '("\\(\\w*:\\)"  . font-lock-variable-name-face))
  "Minimal highlighting expressions for rgbds mode.")
(defconst rgbds-font-lock-keywords-2
  (append rgbds-font-lock-keywords-1
          (list
           '("\\<\\(\\([0-9][0-9A-Fa-f]*[Hh]\\|\\(0[Xx]\\|[0-9]\\|\\$[0-9A-Fa-f]\\)[0-9A-Fa-f]*\\)\\|[01][01]*[Bb]\\|%[01][01]*\\|[0-9]*\\)\\>" . font-lock-constant-face)
           '("\\(\\$\\)" . font-lock-function-name-face)))
  "Additional Keywords to highlight in rgbds mode.")
(defconst rgbds-font-lock-keywords-3
  (append rgbds-font-lock-keywords-2
          (list
           '("\\(\\.\\w*\\|#\\w*\\)" . font-lock-preprocessor-face)
           '("\\<\\(DB\\|DW\\|DS\\|SECTION\\|EQU\\|EQUS\\|SET\\|POPS\\|PUSHS\\|MACRO\\|ENDM\\|RSSET\\|RSRESET\\|RB\\|RW\\|SHIFT\\|EXPORT\\|GLOBAL\\|PURGE\\|INCBIN\\|UNION\\|NEXTU\\|ENDU\\|PRINTT\\|PRINTI\\|PRINTV\\|PRINTF\\|REPT\\|ENDR\\|FAIL\\|WARN\\|INCLUDE\\|IF\\|ELIF\\|ELSE\\|ENDC\\|CHARMAP\\)\\>" . font-lock-preprocessor-face)
           '("\\<\\(db\\|dw\\|ds\\|section\\|equ\\|equs\\|set\\|pops\\|pushs\\|macro\\|endm\\|rsset\\|rsreset\\|rb\\|rw\\|shift\\|export\\|global\\|purge\\|incbin\\|union\\|nextu\\|endu\\|printt\\|printi\\|printv\\|printf\\|rept\\|endr\\|fail\\|warn\\|include\\|if\\|elif\\|else\\|endc\\|charmap\\)\\>" . font-lock-preprocessor-face)))
  "Balls-out highlighting in rgbds mode.")
(defvar rgbds-font-lock-keywords rgbds-font-lock-keywords-3
  "Default highlighting expressions for rgbds mode.")

;; DON'T SREFACTOR THIS SEXP - there's a bug in srefactor that treats semicolons
;; as comments, even when they're not.
(define-derived-mode rgbds-mode
  prog-mode
  "RGBDS"
  "Major mode for Gameboy assembly, to be assembled with rgbasm."
  (setq font-lock-defaults '(rgbds-font-lock-keywords))
  :syntax-table (let ((st (make-syntax-table)))
                  (modify-syntax-entry ?_ "w" st)
                  (modify-syntax-entry ?#"w" st)
                  (modify-syntax-entry ?. "w" st)
                  (modify-syntax-entry ?\; "<" st)
                  (modify-syntax-entry ?\n ">" st)
                  (modify-syntax-entry ?\t "-" st)
                  st))

(define-key rgbds-mode-map (kbd "C-j") 'newline-and-indent)
(define-key rgbds-mode-map (kbd "RET") 'newline-and-indent)
(define-key rgbds-mode-map (kbd "C-a") 'mwim-beginning-of-code-or-line)
(define-key rgbds-mode-map (kbd "C-e") 'mwim-end-of-code-or-line)

(provide 'rgbds-mode)
;;; rgbds-mode ends here
