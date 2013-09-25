(defun hao-pick-current-word ()
  "pick current word under cursor"
  (save-excursion
    (let (head-point tail-point word)
      (skip-chars-forward "-_A-Za-z0-9")
      (setq tail-point (point))
      (skip-chars-backward "-_A-Za-z0-9")
      (setq head-point (point))
      (buffer-substring-no-properties head-point tail-point))))

(defun hao-erlang-pair-commentp ()
  (save-excursion
    (let ((line-head-point (line-beginning-position)))
      (backward-char)
      (catch 'commented
	(while (>= (point) line-head-point)
	  (if (equal (char-after) ?%)
	      (throw 'commented t)
	    (backward-char)))
	nil))))

(defun hao-erlang-pair-construct-stack (value old-stack)
  (if (= 0 (+ value (car old-stack)))
      (cdr old-stack)
    (cons value old-stack)))

(defun hao-erlang-pair-find (direction stack origin-point)
  (let ((new-stack nil))
    (condition-case nil
	(progn
	  (funcall direction "\\(^\\|[\s\t=>]\\)\\(case\\|if\\|begin\\|receive\\|fun[\s\t\n]*(.*)[\s\t\n]*->\\|end\\)\\($\\|[\s\t,;.]\\)")
	  (goto-char (match-beginning 2))
	  (setq new-stack
		(if (hao-erlang-pair-commentp)
		    stack
		  (if (looking-at "end")
		      (hao-erlang-pair-construct-stack -1 stack)
		    (hao-erlang-pair-construct-stack 1 stack))))
	  (when new-stack
	    (forward-char)		; a trick here, there is no need to use
					; (backward-char) here when do backward-search,
					; but you have to use (forward-char) when do forward-search
	    (hao-erlang-pair-find direction new-stack origin-point)))
      (error (progn
	      (message "Wrong format")
	      (goto-char origin-point))))))

(defun hao-erlang-pair ()
  "find pair for if, case, begin for Erlang mode"
  (interactive)
  (let ((keywords '("case" "if" "begin" "receive" "fun")))
    (unless (hao-erlang-pair-commentp)
      (if (member (hao-pick-current-word) keywords)
	  (progn
	    (forward-char)
	    (hao-erlang-pair-find 'search-forward-regexp '(1) (point)))
	(if (equal (hao-pick-current-word) "end")
	    (progn
	      (backward-char)
	      (hao-erlang-pair-find 'search-backward-regexp '(-1) (point))))))))

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

;; add line number
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
(add-hook 'erlang-mode-hook 
          (lambda ()
	    (setq indent-tabs-mode nil)
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

;; kernel style
(add-hook 'c-mode-hook
	  '(lambda ()
	     (c-set-style "linux")
	     ;;(c-set-offset 'case-label '+)
	     (setq indent-tabs-mode nil)
	     (setq c-basic-offset 4)))

;; just for fun
(defun hao-show-system-type ()
  "find out what OS Emacs is currently running on"
  (interactive)
  (message "%s" system-type))

(defun hao-show-emacs-major-version ()
  "find out Emacs major version"
  (interactive)
  (message "%s" emacs-major-version))

(defun hao-show-features-loaded ()
  "find out all features loaded"
  (interactive)
  (message "%S" features))

;; internal function
(defun hao-regexp-word-at-point ()
  "pick current regexp word at point"
  (save-excursion
    (let (head-point tail-point word (skip-chars "-_A-Za-z0-9"))
      (skip-chars-forward skip-chars)
      (setq tail-point (point))
      (skip-chars-backward skip-chars)
      (setq head-point (point))
      (setq word (buffer-substring-no-properties head-point tail-point))
      (concat "\\b" word "\\b"))))

;; highlight word at point
;; bind to [f3]
(defun hao-highlight-word-at-point ()
  "highlight the word at point"
  (interactive)
  (let (regexp-word color hi-colors)
    (unless (boundp 'hao-highlight-word-at-point)
      (setq hao-highlight-word-at-point 0))
    (setq regexp-word (hao-regexp-word-at-point))
    (unhighlight-regexp regexp-word)
    (add-to-list 'regexp-search-ring regexp-word)
    ;; only 4 highlight colors supported now
    (setq hi-colors '("hi-yellow" "hi-pink" "hi-green" "hi-blue"))
    (setq color 
	  (nth (% hao-highlight-word-at-point (length hi-colors)) hi-colors))
    (highlight-regexp regexp-word color)
    (setq hao-highlight-word-at-point (1+ hao-highlight-word-at-point))))
(global-set-key [f3] 'hao-highlight-word-at-point)

(defun hao-unhighlight-all ()
  "unhighlight all highlighted words"
  (interactive)
  ;; in case of a lot of overlays
  (dotimes (i 10)
    (mapc (lambda (regex) (unhighlight-regexp regex))
	(append regexp-history regexp-search-ring))))

(defun hao-unhighlight-word-at-point ()
  "unhighlight the word at point"
  ;; in case of a lot of overlays 
  (interactive)
  (dotimes (i 10)
    (unhighlight-regexp (hao-regexp-word-at-point))))

(defun hao-buffer-menu-friendly ()
  "show buffer menu friendly"
  (interactive)
  (split-window-horizontally)
  (windmove-right)
  (buffer-menu))
(global-set-key (kbd "\C-x \C-b") 'hao-buffer-menu-friendly)

(defun hao-open-window-horizontally-friendly ()
  "open a new window at right side and move into it"
  (interactive)
  (split-window-horizontally)
  (windmove-right))
  (global-set-key (kbd "\C-x 3") 'hao-open-window-horizontally-friendly)
  
(defun hao-open-window-vertically-friendly ()
  "open a new widow at beneth side and move into it"
  (interactive)
  (split-window-vertically)
  (windmove-down))
(global-set-key (kbd "\C-x 2") 'hao-open-window-vertically-friendly)

(defun hao-other-window-backward ()
  "similar to other-window but backward"
  (interactive)
  (other-window -1))
(global-set-key (kbd "M-n") 'other-window)
(global-set-key (kbd "M-p") 'hao-other-window-backward)


