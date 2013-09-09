;; Set Default mode
(setq default-major-mode 'text-mode)

;; can't live without C-h
(define-key key-translation-map [?\C-h] [?\C-?])

;; molokai theme
;; (add-to-list 'load-path "~/.emacs.d/themes/")
;; (require 'molokai-theme)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'molokai t)

;; prevent Emacs from making backup files
(setq backup-inhibited t)
(setq auto-save-default nil)

;; enable line and column numbering
(line-number-mode t)
(column-number-mode t)

;; Add line number
(require 'linum)
(global-linum-mode t)
(defadvice linum-update-window (around linum-format-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat "%" (number-to-string w) "d ")))
    ad-do-it))

;; yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; no menu bar
(menu-bar-mode -1)

;; display time and system load
(display-time)

;; save place
(require 'saveplace)
(setq save-place t)

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (modify-syntax-entry ?- "w")))

;; Emacs Erlang mode setup
;; specify otp-path first
(setq otp-path "/Users/ruan/Library/Erlang/otp16b01")
(add-to-list 'load-path
	     (car (file-expand-wildcards (concat otp-path "/lib/erlang/lib/tools-*/emacs"))))
(setq erlang-root-dir (concat otp-path "/lib/erlang"))
(add-to-list 'exec-path (concat otp-path "/lib/erlang/bin"))
(require 'erlang-start)
(add-hook 'erlang-mode-hook 'erlang-font-lock-level-3)
(add-hook 'erlang-mode-hook 
          (lambda ()
            (erlang-font-lock-level-3)
	    (modify-syntax-entry ?_ "w")
            ;; when starting an Erlang shell in Emacs, default in the node name
            (setq inferior-erlang-machine-options '("-sname" "emacs" "-setcookie" "emacs"))))
(add-to-list 'auto-mode-alist '("\\.\\(erl\\|hrl\\|app\\|app.src\\)" . erlang-mode))


;; distel setup (Emacs Erlang IDE)
(add-to-list 'load-path "~/.emacs.d/bundle/distel/elisp")
(require 'distel)
(distel-setup)
(setq derl-cookie "emacs")

;; parenthesis pair utilities
(show-paren-mode t)
(add-to-list 'load-path "~/.emacs.d/bundle/autopair")
(require 'autopair)
(autopair-global-mode)

;; scroll line by line
(setq scroll-step 1)
(setq scroll-conservatively 9999)

;; enable hi-lock-mode
;; (global-hi-lock-mode 1)

;; just for fun
(defun show-system-type ()
  "find out what OS Emacs is currently running on"
  (interactive)
  (message "%s" system-type))

(defun show-emacs-major-version ()
  "find out Emacs major version"
  (interactive)
  (message "%s" emacs-major-version))

(defun show-features-loaded ()
  "find out all features loaded"
  (interactive)
  (message "%S" features))

;; internal function
(defun pick-current-regexp-word ()
  "pick current word under cursor"
  (save-excursion
    (let (head-point tail-point word)
      (skip-chars-forward "-_A-Za-z0-9")
      (setq tail-point (point))
      (skip-chars-backward "-_A-Za-z0-9")
      (setq head-point (point))
      (setq word (buffer-substring-no-properties head-point tail-point))
      (concat "\\b" word "\\b"))))

(defun highlight-current-word ()
  "highlight the word under cursor"
  (interactive)
  (let (regexp-word color hi-colors)
    (unless (boundp 'highlight-current-word)
      (setq highlight-current-word 0))
    (setq regexp-word (pick-current-regexp-word))
    (unhighlight-regexp regexp-word)
    (add-to-list 'regexp-search-ring regexp-word)
    ;; only 4 highlight colors supported
    (setq hi-colors '("hi-yellow" "hi-pink" "hi-green" "hi-blue"))
    (setq color 
	  (nth (% highlight-current-word (length hi-colors)) hi-colors))
    (highlight-regexp regexp-word color)
    (setq highlight-current-word (1+ highlight-current-word))))
(global-set-key [f3] 'highlight-current-word)

(defun unhighlight-all ()
  "unhighlight all highlighted words"
  (interactive)
  ;; in case of a lot of overlays
  (dotimes (i 10)
    (mapc (lambda (regex) (unhighlight-regexp regex))
	(append regexp-history regexp-search-ring))))

(defun unhighlight-current-word ()
  "unhighlight the word under cursor"
  ;; in case of a lot of overlays
  (interactive)
  (dotimes (i 10)
    (unhighlight-regexp (pick-current-regexp-word))))

(defun buffer-menu-friendly ()
  "show buffer menu friendly"
  (interactive)
  (split-window-horizontally)
  (windmove-right)
  (buffer-menu))
(global-set-key (kbd "\C-x \C-b") 'buffer-menu-friendly)
;;;;;;;;;;;;;;;;;;;;;;;;;;; useful functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (point)
;; (region-beginning)
;; (region-end)
;; (line-beginning-position)
;; (line-end-position)
;; (point-min)
;; (point-max)
;; 
;; (goto-char 1)
;; (forward-char 1)
;; (backward-char 1)
;; 
;; ;; move cursor to the location of "string"
;; ;; returns the new position
;; (search-forward "string")		; end of "string"
;; (search-backward "string")		; beginning of "string"
;; (re-search-forward "string")
;; (re-search-backward "string")
;; ;; move cursor to the first char that's not "a to z"
;; ;; returns the distance traveled
;; (skip-chars-forward "a-z")
;; (skip-chars-backward "a-z")
;; 
;; (delete-char 1)
;; (delete-region start-pos end-pos)
;; (insert "string")
;; ;; get string from current buffer
;; (buffer-substring-no-properties start-pos end-pos)
;; (capitalize-region start-pos end-pos)
;; 
;; (length "string")
;; (substring "string" start-index end-index)
;; (replace-regexp-in-string regex replacement str)
;; 
;; (buffer-name)
;; (buffer-file-name)
;; ;; switch to other buffer
;; ;; this function does not display the buffer in any window, 
;; ;; so the user cannot necessarily see the buffer. 
;; ;; but Lisp programs will now operate on it. 
;; (set-buffer buffer-name)
;; (save-buffer)
;; (kill-buffer buffer-name)
;; (kill-this-buffer)
;; ;; temporarily sets a buffer as current to work with
;; (with-current-buffer buffer-name
;;   ;; do something here)
;; 
;; ;; insert file into current position
;; (insert-file-contents file-path)
;; ;; append a text block to file
;; (append-to-file start-pos end-pos file-path)
;; ;; get dir path
;; (file-name-directory full-file-path)
;; ;; get filename part
;; (file-name-nondirectory full-file-path)
;; ;; get filename's suffix
;; (file-name-extension file-name)
;; (file-name-sans-extension file-name)

