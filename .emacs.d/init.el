;; set default mode
(setq default-major-mode 'text-mode)

;; can't live without C-h
(define-key key-translation-map [?\C-h] [?\C-?])

;; molokai theme
(add-to-list 'load-path "~/.emacs.d/themes/")
(load-theme 'molokai)

;; prevent Emacs from making backup files
(setq backup-inhibited t)
(setq auto-save-default nil)

;; enable line and column numbering
(line-number-mode t)
(column-number-mode t)

;; Add line number
(require 'linum)
(global-linum-mode t)
(setq-default linum-format "%2d ")

;; yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; no menu bar
(menu-bar-mode -1)

;; display time and system load
(display-time)

;; save place
(require 'saveplace)
(setq save-place t)

;; Emacs Erlang mode setup
(add-to-list 'load-path
	     (car (file-expand-wildcards "/usr/local/lib/erlang/lib/tools-*/emacs")))
(setq erlang-root-dir "/usr/local/lib/erlang")
(setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
(require 'erlang-start)
(add-hook 'erlang-mode-hook 'erlang-font-lock-level-3)
(add-hook 'erlang-mode-hook 
          (lambda ()
            (erlang-font-lock-level-3)
            ;; when starting an Erlang shell in Emacs, default in the node name
            (setq inferior-erlang-machine-options '("-sname" "distel" "-setcookie" "distel_cookie"))
            (modify-syntax-entry ?_ "w")))
(add-to-list 'auto-mode-alist '("\\.\\(erl\\|hrl\\|app\\|app.src\\)" . erlang-mode))

;; distel setup (Emacs Erlang IDE)
(add-to-list 'load-path "~/.emacs.d/bundle/distel/elisp")
(require 'distel)
(distel-setup)
(setq derl-cookie "distel_cookie")

;; parenthesis pair utilities
(show-paren-mode t)
(add-to-list 'load-path "~/.emacs.d/bundle/autopair")
(require 'autopair)
(autopair-global-mode)

;; scroll line by line
(setq scroll-step 1)
(setq scroll-conservatively 9999)

;; use C-x w h to highlight and 
;; C-x w r to unhighlight
(global-hi-lock-mode 1)