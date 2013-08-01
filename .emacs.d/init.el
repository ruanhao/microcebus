;; === Set default mode as text-mode ===
(setq default-major-mode 'text-mode)

;; === Set C-h as Backspace ===
(define-key key-translation-map [?\C-h] [?\C-?])

;; === Line by line scrolling ===
(setq scroll-step 1)

;; === Turn off tab character ===
(setq-default indent-tabs-mode nil)

;; === Prevent Emacs from making backup files ===
(setq make-backup-files nil) 

;; === Enable line and column numbering ===
(line-number-mode t)
(column-number-mode t)

;; === Set 'underscore' as a part of a word ===
(modify-syntax-entry ?_ "w")

;; === Add line number ===
(require 'linum)
(global-linum-mode t)
(setq-default linum-format "%2d  ")

;; === Set yes or no ===
(fset 'yes-or-no-p 'y-or-n-p)

;; === Inhibit startup message ===
;; (setq inhibit-startup-message t)

;; === I don't like menu bar ===
(menu-bar-mode -1)
