;;; Basic configuration
(when (version< emacs-version "24.4")
  (error "This requires Emacs 24.4 and above!"))

;; Optimize loading performance
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold 30000000)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after init"
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold 800000)))

(setq inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)

(setq frame-title-format "-- master -- %f -- %b")
(setq ring-bell-function 'ignore)
(setq tab-width 4 indent-tabs-mode nil)

(setq *is-linux* (featurep 'x))
(setq *is-windows* (not *is-linux*))

(setq shift-select-mode t)
; Smooth scroll
(setq scroll-step 3)

;; Recent file number
(setq recentf-max-saved-items 100)

;; For quick find file in git repo
(setq vc-handled-backends ())

;; Set default directory on startup
(setq default-directory "~")

;; Backup directory, file name is !drive_f!dirname!dirname!filename~
(setq backup-directory-alist (quote (("." . "~/emscache"))))

;;(setq url-proxy-services
;;   '(("no_proxy" . "^\\(localhost\\|10.*\\)")
;;     ("http" . "127.0.0.1:1080")
;;     ("https" . "127.0.0.1:1080")))

(show-paren-mode 1)

(tool-bar-mode -1)
(menu-bar-mode -1)
(set-scroll-bar-mode nil)


;; Clock
(display-time)

(setq column-number-mode t)

;; no screwing with my middle mouse button
(global-unset-key [mouse-2])

(global-auto-revert-mode)

;; Remember the cursor position of files when reopening them
(save-place-mode 1)


(setq dabbrev-case-replace t)
(setq dabbrev-case-fold-search t)
(setq dabbrev-upcase-means-case-search t)
(abbrev-mode 1)


;; grep command
(set-variable 'grep-command "grep -irHn ")
(when *is-windows*
    (setq grep-use-null-device t)
    (set-variable 'grep-command "findstr -s -n -i -l "))

;; Set register
(setq register-separator ?+)
(set-register register-separator "\n\n")

(defun xah-comment-dwim ()
  "Like `comment-dwim', but toggle comment if cursor is not at end of line.

URL `http://ergoemacs.org/emacs/emacs_toggle_comment_by_line.html'
Version 2016-10-25"
  (interactive)
  (if (region-active-p)
      (comment-dwim nil)
    (let (($lbp (line-beginning-position))
          ($lep (line-end-position)))
      (if (eq $lbp $lep)
          (progn
            (comment-dwim nil))
        (if (eq (point) $lep)
            (progn
              (comment-dwim nil))
          (progn
            (comment-or-uncomment-region $lbp $lep)
            (forward-line )))))))

;; Copy from prelude config: https://github.com/bbatsov/prelude/blob/master/core/prelude-core.el
(defun prelude-search (query-url prompt)
  "Open the search url constructed with the QUERY-URL.
PROMPT sets the `read-string prompt."
  (browse-url
   (concat query-url
           (url-hexify-string
            (if mark-active
                (buffer-substring (region-beginning) (region-end))
              (read-string prompt))))))

(defmacro prelude-install-search-engine (search-engine-name search-engine-url search-engine-prompt)
  "Given some information regarding a search engine, install the interactive command to search through them"
  `(defun ,(intern (format "prelude-%s" search-engine-name)) ()
       ,(format "Search %s with a query or region if any." search-engine-name)
       (interactive)
       (prelude-search ,search-engine-url ,search-engine-prompt)))

(prelude-install-search-engine "google"     "http://www.google.com/search?q="              "Google: ")
(prelude-install-search-engine "youtube"    "http://www.youtube.com/results?search_query=" "Search YouTube: ")
(prelude-install-search-engine "github"     "https://github.com/search?q="                 "Search GitHub: ")
(prelude-install-search-engine "duckduckgo" "https://duckduckgo.com/?t=lm&q="              "Search DuckDuckGo: ")
(prelude-install-search-engine "bing"       "https://www.bing.com/search?q="               "Bing: ")


(require 'cl) ; If you don't have it already
(defun* get-closest-pathname (&optional (file "Makefile"))
  "Determine the pathname of the first instance of FILE starting from the current directory towards root.
This may not do the correct thing in presence of links. If it does not find FILE, then it shall return the name
of FILE in the current directory, suitable for creation"
  (let ((root (expand-file-name "/"))) ; the win32 builds should translate this correctly
    (expand-file-name file
		      (loop
		       for d = default-directory then (expand-file-name ".." d)
		       if (file-exists-p (expand-file-name file d))
		       return d
		       if (equal d root)
		       return nil))))

(require 'compile)
(if *is-windows*
    (setq my-makescript "build.bat")
  (setq my-makescript "build.linux")
  )
(defun build-command ()
  (set (make-local-variable 'compile-command)
       (get-closest-pathname my-makescript)))
(add-hook 'c++-mode-hook 'build-command)

; Success or failure of compile
(defun notify-compilation-result(buffer msg)
  "Notify that the compilation is finished."
  (if (string-match "^finished" msg)
      (progn
;;	(delete-windows-on buffer) ; Auto close compilation buffer
	(tooltip-show "\n Compilation Successful :-) \n "))
      (tooltip-show "\n Compilation Failed :-( \n ")))
(add-to-list 'compilation-finish-functions 'notify-compilation-result)


(defun my-find-corresponding-file ()
    "Find the file that corresponds to this one."
    (interactive)
    (setq CorrespondingFileName nil)
    (setq BaseFileName (file-name-sans-extension buffer-file-name))
    (if (string-match "\\.c" buffer-file-name)
       (setq CorrespondingFileName (concat BaseFileName ".h")))
    (if (string-match "\\.h" buffer-file-name)
       (if (file-exists-p (concat BaseFileName ".c")) (setq CorrespondingFileName (concat BaseFileName ".c"))
	   (setq CorrespondingFileName (concat BaseFileName ".cpp"))))
    (if (string-match "\\.hin" buffer-file-name)
       (setq CorrespondingFileName (concat BaseFileName ".cin")))
    (if (string-match "\\.cin" buffer-file-name)
       (setq CorrespondingFileName (concat BaseFileName ".hin")))
    (if (string-match "\\.cpp" buffer-file-name)
       (setq CorrespondingFileName (concat BaseFileName ".h")))
	(if (string-match "\\.c" buffer-file-name)
       (setq CorrespondingFileName (concat BaseFileName ".h")))
    (if CorrespondingFileName (find-file CorrespondingFileName)
      (error "Unable to find a corresponding file")))


(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.emacs-china.org/gnu/")
	("melpa" . "http://elpa.emacs-china.org/melpa/")
	("melpa-stable" . "http://elpa.emacs-china.org/melpa-stable/")))
(package-initialize)

;; On-demand installation of packages
(defun require-package (package &optional min-version no-refresh)
  "Ask elpa to install given PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package dash
  :ensure t)

(use-package flx-ido
  :ensure t
  :init
  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1)
  ;; disable ido faces to see flx highlights.
  ;;(setq ido-enable-flex-matching t)
  ;;(setq ido-use-faces nil)
  )

(use-package ido-vertical-mode
  :ensure t
  :config
  (progn
    (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
    (setq ido-vertical-show-count t)
    (setq ido-use-faces t)
    (set-face-attribute 'ido-vertical-first-match-face nil
			:background "#e5b7c0")
    (set-face-attribute 'ido-vertical-only-match-face nil
			:background "#e52b50"
			:foreground "white")
    (set-face-attribute 'ido-vertical-match-face nil
			:foreground "#b00000")
    (ido-vertical-mode 1)))

(use-package smex
  :ensure t
  :init
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'execute-extended-command))


;; diminish, Diminished modes are minor modes with no modeline display
(use-package diminish :ensure t)

(use-package swiper
  :ensure t
  :config
  (progn
    (global-set-key "\C-s" 'swiper)))

;; which-key
(use-package which-key
             :ensure t
             :config
             (which-key-mode)
             (diminish 'which-key-mode)
             (which-key-setup-side-window-bottom)
	     ;(which-key-setup-minibuffer)
	     )


(use-package cnfonts
  :ensure t
  :init
  (cnfonts-enable))

;; avy, Jump to arbitrary positions in visible text and select text quickly.
(use-package avy
  :ensure t)

(use-package counsel
  :ensure t
  )

(use-package magit
  :ensure t)

;; projectile
(use-package projectile
  :ensure t
  :init
  (projectile-global-mode)
  (setq projectile-enable-caching 1)
  (diminish 'projectile-mode "prj"))

(use-package indent-guide
  :ensure t
  :init
  (indent-guide-global-mode))

(use-package yasnippet
  :ensure t
  :init
;;  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippetsts")
  (yas-global-mode 1))


(use-package color-identifiers-mode
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-color-identifiers-mode))


(use-package autopair
  :ensure t
  :init
  (autopair-global-mode)
  (diminish 'autopair-mode))


(use-package racket-mode
  :ensure t
  :init
  (add-hook 'racket-mode-hook
          (lambda ()
	    (define-key racket-mode-map (kbd "<f5>") 'racket-send-last-sexp)
            (define-key racket-mode-map (kbd "C-<f5>") 'racket-run))))


(use-package multiple-cursors
  :ensure t)


;; ws-butler, Unobtrusively remove trailing whitespace.
(use-package ws-butler
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'ws-butler-mode)
  (add-hook 'text-mode 'ws-butler-mode)
  (add-hook 'fundamental-mode 'ws-butler-mode)
  (diminish 'ws-butler-mode))


;; anzu, Show number of matches in mode-line while searching.
(use-package anzu
  :ensure t
  :init
  (global-anzu-mode)
  (diminish 'anzu-mode))


;; wgrep-ag, Writable grep buffer and apply the changes to files
(use-package wgrep-ag
  :ensure t
  :requires wgrep
  :init
  ;; To save buffer automatically when wgrep-finish-edit.
  (setq wgrep-auto-save-buffer t))


;; company, auto complete
(use-package company
  :ensure
  :init
  (setq company-show-numbers t) ; use alt+number to quick select
  (setq company-idle-delay 0.2) ; immediately company complete
  (setq company-selection-wrap-around t) ; make previous/next selection in the popup cycles
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-limit 20)
  (setq company-backends nil)
  (add-to-list 'company-backends 'company-keywords)
  (add-to-list 'company-backends 'company-elisp)
  (add-to-list 'company-backends 'company-files)
  (add-to-list 'company-backends 'company-etags)
  ;; company-dabbrev config, it is for current buffer string auto complete.
  (add-to-list 'company-backends 'company-dabbrev)
  (add-to-list 'company-backends 'company-dabbrev-code)
  (setq company-dabbrev-code-everywhere t)
  (setq company-dabbrev-minimum-length 2)
  (setq company-dabbrev-other-buffers 'all)
  (setq company-dabbrev-downcase nil)
  (add-hook 'after-init-hook 'global-company-mode))

;; company-statistics, Sort candidates using completion history
(use-package company-statistics
  :ensure t
  :requires company
  :init
  (company-statistics-mode))


(use-package company-c-headers
  :ensure t
  :requires company
  :config
  (progn
    (add-to-list 'company-backends 'company-c-headers)
    (add-hook 'after-init-hook 'global-company-mode)
    (diminish 'company-mode "com")))


;; eno, Goto/copy/cut any word/symbol/line in view, similar to ace-jump/easymotion
(use-package eno
  :ensure t)


(use-package w32-browser
  :ensure t
  :init
  (define-key dired-mode-map [f11] 'dired-w32-browser))


;;(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
;;(require 'init-site-lisp)
;;
;;(when (and *is-windows* (file-exists-p "~/.emacs.d/site-lisp/everything/everything.el"))
;;  (setq everything-ffap-integration nil)
;;  ;; to disable ffap integration
;;  (setq everything-cmd "D:/Software/Everything-1.4.1.877.x64/es.exe")
;;  ;; to let everything.el know where to find es.exe
;;  (require 'everything))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; \e means escape
(global-set-key "\e " 'set-mark-command)

;; Quick copy/cut current line
(global-set-key "\C-c\C-c" "\C-a\C- \C-n\M-w")
(global-set-key "\C-c\C-x" "\C-a\C- \C-n\C-w")

(global-set-key (kbd "M-;") 'xah-comment-dwim)

(global-set-key (kbd "<M-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<M-wheel-down>") 'text-scale-decrease)

;;(global-set-key (kbd "<M-up>") 'previous-buffer)
;;(global-set-key (kbd "<M-down>") 'next-buffer)

(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key "\C-x\C-f" 'counsel-find-file)
;;(global-set-key "\M-x" 'counsel-M-x)

;; Magit
(global-set-key '[(f4)]		'magit-status)
(global-set-key '[(C-f4)]	'magit-checkout-file)
(global-set-key '[(S-f4)]	'magit-stage-file)
(global-set-key '[(C-M-f4)]	'magit-commit)


(global-set-key '[(f5)] 'compile)


;; ediff
(global-set-key '[(f7)]          'ediff-directories)
(global-set-key '[(S-f7)]        'ediff-buffers)
(global-set-key '[(C-f7)]        'ediff-files)
(global-set-key '[(C-M-f7)]      'ediff-regions-linewise)


(global-set-key '[(f1)] 'ido-find-file)
(global-set-key '[(C-f1)] 'ido-find-file-other-window)
(global-set-key '[(S-f1)] 'dired)

(global-set-key '[(f2)] 'other-window)
(global-set-key '[(C-f2)] 'ido-switch-buffer)
(global-set-key '[(S-f2)] 'ido-switch-buffer-other-window)

(global-set-key '[(f3)] 'split-window-horizontally)
(global-set-key '[(f3)] 'split-window-vertically)


;; Maximizing on startup
(defun maximize-frame ()
  "Maximizes the active frame in Windows"
  (interactive)
  ;; Send a `WM_SYSCOMMAND' message to the active frame with the
  ;; `SC_MAXIMIZE' parameter.
  (when (eq system-type 'windows-nt)
    (w32-send-sys-command 61488)))

(defun post-load-stuff ()
  (interactive)
  (menu-bar-mode -1)
  (maximize-frame)
;;  (set-foreground-color "burlywood3")
;;  (set-background-color "black")
  (set-cursor-color "#40FF40")
)
(add-hook 'window-setup-hook 'post-load-stuff t)
(add-hook 'window-setup-hook 'toggle-frame-maximized t)



(add-to-list 'default-frame-alist '(font . "Liberation Mono-11.5"))
(set-face-attribute 'default t :font "Liberation Mono-11.5")
;;(set-face-attribute 'font-lock-builtin-face nil :foreground "#DAB98F")
;;(set-face-attribute 'font-lock-comment-face nil :foreground "gray50")
;;(set-face-attribute 'font-lock-constant-face nil :foreground "olive drab")
;;(set-face-attribute 'font-lock-doc-face nil :foreground "gray50")
;;(set-face-attribute 'font-lock-function-name-face nil :foreground "burlywood3")
;;(set-face-attribute 'font-lock-keyword-face nil :foreground "DarkGoldenrod3")
;;(set-face-attribute 'font-lock-string-face nil :foreground "olive drab")
;;(set-face-attribute 'font-lock-type-face nil :foreground "burlywood3")
;;(set-face-attribute 'font-lock-variable-name-face nil :foreground "burlywood3")

;; Custom word face
(setq fixme-modes '(c++-mode c-mode emacs-lisp-mode))
(make-face 'font-lock-fixme-face)
(make-face 'font-lock-study-face)
(make-face 'font-lock-important-face)
(make-face 'font-lock-improve-face)
(make-face 'font-lock-note-face)
(mapc (lambda (mode)
	 (font-lock-add-keywords
	  mode
	  '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
	    ("\\<\\(STUDY\\)" 1 'font-lock-study-face t)
		("\\<\\(IMPROVE\\)" 1 'font-lock-improve-face t)
	    ("\\<\\(IMPORTANT\\)" 1 'font-lock-important-face t)
           ("\\<\\(NOTE\\)" 1 'font-lock-note-face t))))
	fixme-modes)
(modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-study-face "Green" nil nil t nil t nil nil)
(modify-face 'font-lock-improve-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-important-face "Green" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "Dark Green" nil nil t nil t nil nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ido-vertical-mode w32-browser eno company-c-headers company-statistics company wgrep-ag anzu ws-butler multiple-cursors racket-mode autopair color-identifiers-mode yasnippet indent-guide projectile magit avy cnfonts which-key swiper diminish smex flx-ido dash use-package bind-key))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
