;; Set default mode as text-mode.
(setq default-major-mode 'text-mode)

;; Set C-h as Backspace.
(define-key key-translation-map [?\C-h] [?\C-?])

;; Line by line scrollin.g
;; (setq scroll-step 1)

;; Turn off tab character.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Prevent Emacs from making backup files.
(setq backup-inhibited t)
(setq auto-save-default nil)

;; Enable line and column numbering.
(line-number-mode t)
(column-number-mode t)

;; Set 'underscore' as a part of a word.
;; (modify-syntax-entry ?_ "w")

;; Add line number.
(require 'linum)
(global-linum-mode t)
(setq-default linum-format "%2d  ")

;; Set yes or no.
(fset 'yes-or-no-p 'y-or-n-p)

;; I don't like menu bar.
(menu-bar-mode -1)

;; Display time and system load.
(display-time)

;; When you visit a file, point goes to the last place 
;; where it was when you previously visited the same file.
(require 'saveplace)
(setq-default save-place t)

;; Select current word.
;; Pressing it once will select the current whole word. 
;; Press it again will extend the selection to the next outer parens.
;; By Nikolaj Schumacher, 2008-10-20. Released under GPL.
(defun semnav-up (arg)
  (interactive "p")
  (when (nth 3 (syntax-ppss))
    (if (> arg 0)
        (progn
          (skip-syntax-forward "^\"")
          (goto-char (1+ (point)))
          (decf arg))
      (skip-syntax-backward "^\"")
      (goto-char (1- (point)))
      (incf arg)))
  (up-list arg))

(defun extend-selection (arg &optional incremental)
  "Select the current word.
Subsequent calls expands the selection to larger semantic unit."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (or (region-active-p)
                         (eq last-command this-command))))
  (if incremental
      (progn
        (semnav-up (- arg))
        (forward-sexp)
        (mark-sexp -1))
    (if (> arg 1)
        (extend-selection (1- arg) t)
      (if (looking-at "\\=\\(\\s_\\|\\sw\\)*\\_>")
          (goto-char (match-end 0))
        (unless (memq (char-before) '(?\) ?\"))
          (forward-sexp)))
      (mark-sexp -1))))

(global-set-key (kbd "M-8") 'extend-selection)


;; Select just text inside quotes in one shot.
(defun select-text-in-quote ()
  "Select text between the nearest left and right delimiters.
Delimiters are paired characters:
 () [] {} «» ‹› “” 〖〗 【】 「」 『』 （） 〈〉 《》 〔〕 ⦗⦘ 〘〙 ⦅⦆ 〚〛 ⦃⦄
 For practical purposes, also: \"\", but not single quotes."
 (interactive)
 (let (p1)
   (skip-chars-backward "^<>([{“「『‹«（〈《〔【〖⦗〘⦅〚⦃\"")
   (setq p1 (point))
   (skip-chars-forward "^<>)]}”」』›»）〉》〕】〗⦘〙⦆〛⦄\"")
   (set-mark p1)
   )
 )

(global-set-key (kbd "M-*") 'select-text-in-quote)

;; Select current line.
(defun select-current-line ()
  "Select the current line"
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))

