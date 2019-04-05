

;;(require 'auto-save)
;;(auto-save-enable)
;;(setq auto-save-silent t)
;; (setq auto-save-delete-trailing-whitespace t)

(require 'super-save)
(super-save-mode +1)
(setq super-save-remote-files nil)


;; save minibuffer history
(require 'savehist)
(add-hook 'after-init-hook 'savehist-mode)
(setq enable-recursive-minibuffers t ; Allow commands in minibuffers
      history-length 100
      savehist-additional-variables '(mark-ring
                                      global-mark-ring
                                      search-ring
                                      regexp-search-ring
                                      extended-command-history)
      savehist-autosave-interval nil ;;不开启自动保存，否则会不断的分配内存
      )


(require 'youdao-dictionary)

(autoload 'mpg123 "mpg123" "A Front-end to mpg123/ogg123" t)


(defun eye/open-thunar ()
  "Open thunar of current buffer directory."
  (when (and (executable-find "thunar")
             (not (null default-directory)))
    (start-process "File manager" nil "thunar" default-directory)))

(defun eye/open-explorer ()
  "Open explorer of current buffer directory."
  (when (and (not (string-empty-p default-directory))
	     (eq system-type 'windows-nt))
    (let ((dir default-directory))
      (setq dir (encode-coding-string
		 (replace-regexp-in-string "/" "\\\\" dir) 'gbk-dos))
      (shell-command (concat "explorer " dir) nil nil)
      (message dir))
    ))

(defun eye/open-file-manager ()
  "Open external file manager."
  (interactive)
  (cond ((eq system-type 'windows-nt)
	 (eye/open-explorer))
	((eq system-type 'gnu/linux)
	 (eye/open-thunar))
	(t (message "Not support current system."))
	))

(defun eye/open-terminal ()
  (interactive)
  (when (executable-find "xfce4-terminal")
    (start-process "Terminal" nil "xfce4-terminal")))

;;;; theme
(require 'moe-theme)
(load-theme 'moe-dark t)


;;;; Font
(when is-gui
  (require 'init-font))


;; Custom keyword hightlight
(setq fixme-modes '(c++-mode c-mode emacs-lisp-mode python-mode))
(make-face 'font-lock-fixme-face)
(make-face 'font-lock-study-face)
(make-face 'font-lock-important-face)
(make-face 'font-lock-improve-face)
(make-face 'font-lock-note-face)
(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
           ("\\<\\(IMPROVE\\)" 1 'font-lock-improve-face t)
           ("\\<\\(STUDY\\)" 1 'font-lock-study-face t)
           ("\\<\\(NOTE\\)" 1 'font-lock-note-face t))))
      fixme-modes)
(modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-improve-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-study-face "#33aa00" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "#33aa00" nil nil t nil t nil nil)

;; (setq truncate-lines t) ;; 不自动折行

;;;; Search
(require 'color-rg)

;;;; ivy
(require 'swiper)

(defun swiper-dwim ()
  "Search input word or current select string"
  (interactive)
  (if (region-active-p)
          (let ((str (buffer-substring (region-beginning) (region-end))))
                (pop-mark)
                (swiper str))
        (swiper)))


;; 安装了 smex 后，counsel-M-x 才会按照使用频率排序
(require 'smex)
(require 'counsel)

(defun execute-func-use-marked (func)
  (if (region-active-p)
      (let ((str (buffer-substring (region-beginning) (region-end))))
        (pop-mark)
        (funcall func str))
    (funcall func)))

(defun counsel-rg-marked()
  "Search input word or current select string use rg"
  (interactive)
  (execute-func-use-marked 'counsel-rg))

(defun counsel-ag-marked ()
  "Search input word or current select string use ag"
  (interactive)
  (execute-func-use-marked 'counsel-ag))
;; 如果需要输入长度小于3时不搜索，需要修改内部函数 counsel-ag-function

(require 'ivy)
(setq ivy-initial-inputs-alist nil) ;;不需要自动添加^符号
;; 在当前光标处弹出ivy
;; (setq ivy-completion-beg 0)
;; (setq ivy-display-function 'ivy-display-function-overlay)
(define-key ivy-minibuffer-map (kbd "M-i") 'ivy-previous-line)
(define-key ivy-minibuffer-map (kbd "M-k") 'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "M-o") 'ivy-occur)


;; 不想让分割左右窗口后还是在左下角弹出ivy @see https://emacs-china.org/t/topic/5754/9
(setq ivy-count-format "")
(defvar maple/ivy-format-padding nil)

(defun maple/ivy-read-around (-ivy-read &rest args)
  "Advice ivy-read `-IVY-READ` `ARGS`."
  (let ((maple/ivy-format-padding (make-string (window-left-column) ?\s)))
    (setcar args (concat maple/ivy-format-padding (car args)))
    (apply -ivy-read args)))

(advice-add 'ivy-read :around #'maple/ivy-read-around)

(defun maple/ivy-format-function (cands)
  "Transform CANDS into a string for minibuffer."
  (ivy--format-function-generic
   (lambda (str)
     (concat maple/ivy-format-padding (ivy--add-face str 'ivy-current-match)))
   (lambda (str)
     (concat maple/ivy-format-padding str))
   cands "\n"))

;(setq ivy-format-function 'maple/ivy-format-function)


;; fixed minibuffe
;(setq resize-mini-windows nil)
;(setq ivy-height 6)

;(defun eye/set-mini-window-height (&optional frame)
;  (interactive)
;  (let ((mini-win (minibuffer-window frame)))
;	(when (and mini-win (< (window-size mini-win) ivy-height))
;	  (window-resize mini-win ivy-height))))

;; (add-hook 'window-setup-hook 'eye/set-mini-window-height)
;; (add-hook 'after-make-frame-functions 'eye/set-mini-window-height)
;; (add-hook 'move-frame-functions 'eye/set-mini-window-height)
;; no need above hook if use code below
;(defun eye/mini-ivy-on ()
;  (interactive)
;  (add-hook 'window-size-change-functions 'eye/set-mini-window-height)
;  (add-hook 'after-init-hook 'eye/set-mini-window-height))
;(defun eye/mini-ivy-off ()
;  (interactive)
;  (remove-hook 'window-size-change-functions 'eye/set-mini-window-height)
;  (remove-hook 'after-init-hook 'eye/set-mini-window-height))
			
;; ivy-posframe
(when (and is-gui (>= emacs-major-version 26))
  (require 'ivy-posframe)
  ;; (setq ivy-display-function #'ivy-posframe-display)
  ;; (setq ivy-display-function #'ivy-posframe-display-at-frame-center)
  ;; (setq ivy-display-function #'ivy-posframe-display-at-window-center)
  (setq ivy-display-function #'ivy-posframe-display-at-frame-bottom-left)
  ;; (setq ivy-display-function #'ivy-posframe-display-at-window-bottom-left)
  ;; (setq ivy-display-function #'ivy-posframe-display-at-point)

  (push '(counsel-M-x . ivy-posframe-display-at-window-bottom-left) ivy-display-functions-alist)
  (push '(complete-symbol . ivy-posframe-display-at-point) ivy-display-functions-alist)
  (push '(swiper . ivy-posframe-display-at-point) ivy-display-functions-alist)

  ;; show border
  (set-face-attribute 'internal-border nil :background "gray50")
  (setq ivy-posframe-border-width 1)
  (ivy-posframe-enable)
  )


;;;; Project
(unless (equal system-type 'windows-nt)
  (require 'find-file-in-project))

;; 目录有变化时及时更新
(require 'async)
(require 'dired)
(setq dired-async-mode 1)

(defun dired-dotfiles-toggle ()
  "Show/hide dot-files"
  (interactive)
  (when (equal major-mode 'dired-mode)
        (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p) ; if currently showing
                (progn 
                  (set (make-local-variable 'dired-dotfiles-show-p) nil)
                  (message "h")
                  (dired-mark-files-regexp "^\\\.")
                  (dired-do-kill-lines))
          (progn (revert-buffer) ; otherwise just revert to re-show
                         (set (make-local-variable 'dired-dotfiles-show-p) t)))))

(define-key dired-mode-map (kbd "<f12>s") 'dired-dotfiles-toggle)

;; 使用 windows 程序打开文件
;;(when (eq system-type 'windows-nt)
;;  (require 'w32-browser)
;;  (define-key dired-mode-map [f11] 'dired-w32-browser))

;;;; magit
(when (and (not (equal system-type 'windows-nt)) (executable-find "git"))
  (require 'magit)
  (setq magit-push-always-verify nil)
  (setq git-commit-summary-max-length 80)

  ;; 在新 frame 中打开 magit-status
  (defun magit-display-buffer-pop-up-frame (buffer)
    (if (with-current-buffer buffer (eq major-mode 'magit-status-mode))
        (display-buffer buffer
                        '((display-buffer-reuse-window
                           display-buffer-pop-up-frame) ;; 在新的 frame 中显示
                          (reusable-frames . t)))
      (magit-display-buffer-traditional buffer))) ;; magit-display-buffer-traditional 是默认的函数

  ;; 设置显示 magit buffer 的函数
  (setq magit-display-buffer-function #'magit-display-buffer-pop-up-frame)

  (define-key magit-mode-map (kbd "q") 'delete-frame) ;; 自动关闭 frame
  ;; 添加 magit-submodule-remove
  ;;(require 'magit-extension)
  )

;;;; Shell
(require 'aweshell)
;; (setq show-paren-style 'expression)     ;高亮括号整体内容


;; Show color of #hex format string.
(require 'rainbow-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-mode)

;; fix warning: ad-handle-definition: ‘er/expand-region’ got redefined
;; (setq ad-redefinition-action 'accept)
;; (use-package expand-region
  ;; :ensure t
  ;; :bind ("C-q" . er/expand-region)
  ;; )

(require 'hungry-delete)
(global-hungry-delete-mode)

(require 'wdired)

(require 'dired-x) ;; 支持 dired-jump 进入后自动定位到当前文件名位置
;; 打开 .dired 后缀文件时，自动进入 dired-virtual-mode 模式。
(setq auto-mode-alist (cons '("[^/]\\.dired$" . dired-virtual-mode)
                            auto-mode-alist))

;; 如果开启了全局 global-auto-revert，则 dired-virtual-mode 模式下经常会弹出提示，所以只在编程模式下开启。
(add-hook 'prog-mode-hook
                  '(lambda ()
                         (auto-revert-mode 1)))

;; 隐藏 dired 中文件拥有者和文件权限等信息
(defun eye-dired-mode-setup ()
  "hide the file's unix owner and permission info"
  (dired-hide-details-mode 1))
(add-hook 'dired-mode-hook 'eye-dired-mode-setup)

(require 'wgrep)
(require 'wgrep-ag)


(require 'helm)
(require 'helm-mode)
(helm-mode 1)

(defun eye/helm-hide-minibuffer ()
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))
(add-hook 'helm-minibuffer-set-up-hook 'eye/helm-hide-minibuffer)
(setq helm-autoresize-max-height 0
      helm-autoresize-min-height 40
      helm-M-x-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t
      helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match t
      helm-split-window-in-side-p nil
      helm-move-to-line-cycle-in-source nil
      helm-ff-search-library-in-sexp t
      helm-scroll-amount 8 
      helm-echo-input-in-header-line t)



;;(require 'helm-config)    
;;(helm-autoresize-mode 1)
;;(define-key helm-find-files-map (kbd "C-b") 'helm-find-files-up-one-level)
;;(define-key helm-find-files-map (kbd "C-f") 'helm-execute-persistent-action)

(require 'avy)


(require 'multiple-cursors)

;; 快速复制/剪切/移动其它位置的单词/行
(require 'eno)
(defun eye/eno-copy ()
  (interactive)
  (cond
   ((equal major-mode 'c++-mode)
    (eno-word-copy))
   ((equal major-mode 'emacs-lisp-mode)
    (eno-symbol-copy))))

;; writeroom
(require 'writeroom-mode)
(setq writeroom-width 120)

(defun writeroom-mode-on ()
  (interactive)
  (add-hook 'c++-mode-hook 'writeroom-mode)
  (add-hook 'emacs-lisp-mode-hook 'writeroom-mode)
  (add-hook 'org-mode-hook 'writeroom-mode)
  (add-hook 'css-mode-hook 'writeroom-mode)
  (writeroom-mode))

(defun writeroom-mode-off ()
  (interactive)
  (remove-hook 'c++-mode-hook 'writeroom-mode)
  (remove-hook 'emacs-lisp-mode-hook 'writeroom-mode)
  (remove-hook 'org-mode-hook 'writeroom-mode)
  (remove-hook 'css-mode-hook 'writeroom-mode)
  (writeroom-mode -1))

(require 'watch-other-frame)

;;;; Company
;; (require 'init-company)

;;;; Python
(require 'init-python)

;;;; cpp and qt configuration
(require 'init-cpp)

;;;; yasnippet
(require 'yasnippet)

;;;; Qt
(require 'init-qt)

;;;; elisp
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'yas-minor-mode)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
	    (unless (equal system-type 'windows-nt)
              (require 'company-elisp)
              (add-to-list 'company-backends 'company-elisp))
	    (outline-minor-mode 1)
	    (setq outline-regexp ";;;;+")
	    ))

;; http://ergoemacs.org/emacs/elisp_traverse_dir.html
;; (defun git-submodule-add ()
  ;; (interactive)
  ;; (let ((defualt-dir default-directory))
    ;; (setq default-directory user-emacs-directory)
    ;; (async-shell-command "git submodule add https://github.com/bbatsov/projectile site-lisp/projectile")))


(defun compile-package ()
  (interactive)
  (let ((dir (read-directory-name "Package dir:" "~/emacs-config/site-lisp")))
    (mapc (lambda (x) (byte-compile-file x))
          (directory-files-recursively dir "\.el$" ))))


;;;; php
(require 'php-mode)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
;; fix irony warning when open php file
;; (push 'php-mode irony-supported-major-modes)
(defun eye/switch-php-html-mode ()
  "Switch the php-mode and html-mode"
  (interactive)
  (cond ((string= mode-name "html")
         (php-mode))
        ((string= mode-name "php")
         (html-mode))))

(unless (equal system-type 'windows-nt)
  (require 'company-php)
  (add-hook 'php-mode
            '(lambda ()
               (add-to-list 'company-backends 'company-php)))
  )
(message "require company finished")

;;;; lua
(require 'init-lua)

;;;; sql
(add-hook 'sql-mode-hook 'yas-minor-mode)

;;;; xml
(require 'nxml-mode)
(message "require nxml-mode finished")

;;;; Navigation
(require 'backward-forward)

(require 'dumb-jump)
(with-eval-after-load 'counsel-etags
  (progn
    '(advice-add 'counsel-etags-find-tag-at-point :before #'backward-forward-push-mark-wrapper)
    (advice-add 'dumb-jump-go :before #'backward-forward-push-mark-wrapper)
    ))
(backward-forward-mode t)

;; (require 'init-tags)

(require 'ace-jump-mode)


;; ibuffer
(setq ibuffer-saved-filter-groups
      '(("Default"
         ("Hidden(g则不显示此分组)"  (name . "^ "))
         ("Helm"  (or (name . "^\\*helm\\|^\\*ac-mode-")))
         ("Help"  (or (name . "^\\*help\\|^\\*ac-mode-")))
         ("Woman"  (name . "^\\*WoMan.*\\*$"))
         ("Compile"  (name . "^*.*compil[ea].*$"))
         ("Gtalk"  (or (name . "^\\*.*jabber") (name . "*fsm-debug*")))
         ("ERC"  (mode . erc-mode))
         ("Custom"  (mode . Custom-mode))
         ("Shell"  (mode . shell-mode))
         ("Mail" (or (mode . mew-summary-mode) (mode . mew-draft-mode)(mode . mew-message-mode)))
         ("VC"  (or (name . "*magit-") (name . "^\\*vc")(mode . diff-mode) (mode . vc-dir-mode)))
         ("Magit "  (name . "*magit:"))
         ("Emacs"  (name . "^\\*.*$"))
         ("Dired"  (mode . dired-mode))
         ("Go"  (mode . go-mode))
         ("Python"  (mode . python-mode))
         ("EL"  (or (mode . emacs-lisp-mode) (mode . lisp-interaction-mode)))
         ("C++" (mode . c++-mode))
         ("Text" (name . ".txt"))
         )))

(add-hook 'ibuffer-mode-hook
            '(lambda ()
                (ibuffer-auto-mode 1)
                (ibuffer-switch-to-saved-filter-groups "EL")))
(setq ibuffer-show-empty-filter-groups nil)

;;;; web
(require 'web-mode)


(defun eye/html-char-to-ltgt()
  (interactive)
  (if (use-region-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (replace-string "<" "&lt;" nil start end)
        (goto-char start)
        (replace-string ">" "&gt;" nil start end)
        )))

(defun eye/html-ltgt-to-char()
  (interactive)
  (if (use-region-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (replace-string "&lt;" "<" nil start end)
        (goto-char start)
        (replace-string "&gt;" ">" nil start end)
        )))

;;;; orgmode
(ignore-errors (require 'init-orgmode))


;;;; SESSION
;; desktop save
(require 'desktop)

;; use only one desktop
(setq desktop-dirname user-cache-directory)
(setq desktop-base-file-name "emacs-desktop")

;; remove desktop after it's been read
(add-hook 'desktop-after-read-hook
	  '(lambda ()
	     ;; desktop-remove clears desktop-dirname
	     (setq desktop-dirname-tmp desktop-dirname)
	     (desktop-remove)
	     (setq desktop-dirname desktop-dirname-tmp)))

(defun saved-session ()
  (file-exists-p (concat desktop-dirname "/" desktop-base-file-name)))

;; use session-restore to restore theme desktop manually
(defun session-restore ()
  "Restore a saved emacs session."
  (interactive)
  (if (saved-session)
      (desktop-read user-cache-directory)
    (message "No desktop found.")))

;;(require 'auto-save)
;; use session-save to save the desktop manually
(defun session-save ()
  "Save an emacs session."
  (interactive)
  (auto-save-buffers)
  (if (saved-session)
      (if (y-or-n-p "Overwrite existing desktop? ")
	  (desktop-save-in-desktop-dir)
	(message "Session not saved."))
  (desktop-save-in-desktop-dir)))

;; ask user whether to restore desktop at start-up
;; (add-hook 'after-init-hook
;; 	  '(lambda ()
;; 	     (if (saved-session)
;; 		 (if (y-or-n-p "Restore desktop? ")
;; 		     (session-restore)))))

;;(add-hook 'kill-emacs-hook 'session-save)

;;;; remote
(if is-windows)
    (setq tramp-default-method "plink")
  (setq tramp-default-method "ssh"))

;;;; docs
(require 'helm-dash)
(setq helm-dash-browser-func 'eww)
(setq helm-dash-docsets-path "~/.docset")
(setq helm-dash-common-docsets '("C" "C++" "Qt_5" "Emacs_Lisp"))

;;;; keys
(global-unset-key (kbd "<f1>"))
(global-unset-key (kbd "<f2>"))
(global-unset-key (kbd "<f3>"))
(global-unset-key (kbd "<f4>"))
(global-unset-key (kbd "<f5>"))
(global-unset-key (kbd "<f6>"))
(global-unset-key (kbd "<f7>"))
(global-unset-key (kbd "<f8>"))
(global-unset-key (kbd "<f9>"))
(global-unset-key (kbd "<f10>"))
(global-unset-key (kbd "<f11>"))
(global-unset-key (kbd "<f12>"))


(define-key global-map (kbd "<C-up>") 'scroll-down-command)
(define-key global-map (kbd "<C-down>") 'scroll-up-command)

;; 不设置为全局,否则影响minibuffer输入
;; (define-key prog-mode-map (kbd "<tab>") 'indent-or-expand)
(define-key prog-mode-map (kbd "<tab>") 'hippie-expand)

(define-key global-map (kbd "<backtab>") 'indent-for-tab-command) ;; shift-tab

(defalias 'backward-kill-word 'eye/kill-inner-word)
(define-key global-map (kbd "<M-backspace>") 'eye/kill-inner-word)
(define-key global-map (kbd "<C-backspace>") 'eye/kill-inner-word)

(define-key org-src-mode-map (kbd ",fs") 'org-edit-src-save)

(define-key global-map (kbd "<C-wheel-up>") 'eye/increase-font-size)
(define-key global-map (kbd "<C-wheel-down>") 'eye/decrease-font-size)

(if (>= emacs-major-version 26)
    (global-set-key (kbd "<f1>") 'global-display-line-numbers-mode))


(global-set-key (kbd "<f3> u") 'winner-undo)
(global-set-key (kbd "<f3> i") 'winner-redo

(setq eye-leader-key ",")


(provide 'init-modules)
