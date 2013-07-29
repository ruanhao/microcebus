;; === Set default mode as text-mode ===
(setq default-major-mode 'text-mode)

;; === Set C-h as Backspace ===
(define-key key-translation-map [?\C-h] [?\C-?])

;; === Line by line scrolling ===
;; This makes the buffer scroll by only a single line when the up or
;; down cursor keys push the cursor (tool-bar-mode) outside the
;; buffer. The standard emacs behaviour is to reposition the cursor in
;; the center of the screen, but this can make the scrolling confusing
(setq scroll-step 1)

;; === Turn off tab character ===
;; Emacs normally uses both tabs and spaces to indent lines. If you
;; prefer, all indentation can be made from spaces only. To request this,
;; set 'indent-tabs-mode' to 'nil'. This is a per-buffer variable;
;; altering the variable affects only the current buffer, but it can be
;; disabled for all buffers.
;; Use (setq ...) to set value locally to a buffer
;; Use (setq-default ...) to set value globally 
(setq-default indent-tabs-mode nil)

;; === Prevent Emacs from making backup files ===
(setq make-backup-files nil) 

;; === Enable Line and Column Numbering ===
;; Show line-number in the mode line
(line-number-mode t)
;; Show column-number in the mode line
(column-number-mode t)

;; === Set 'underscore' as a part of a word ===
(modify-syntax-entry ?_ "w")

;; === Add line number and set xterm-mouse-mode ===
(require 'linum)
(global-linum-mode t)
(when (not window-system)
  (setq-default linum-format "%2d  ")
  (xterm-mouse-mode t))
(defun up-slightly () (interactive) (scroll-up 3))
(defun down-slightly () (interactive) (scroll-down 3))
(global-set-key [mouse-4] 'down-slightly)
(global-set-key [mouse-5] 'up-slightly)
