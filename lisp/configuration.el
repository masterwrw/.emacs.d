;;; configuration.el --- My emacs configuration -*- lexical-binding: t -*-

;;;; debug
;;Produce backtraces when errors occur
(setq debug-on-error t)

;;;; version check
(let ((minver "24.4"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "25.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

;;;; startup
(setq gc-cons-threshold 80000000) ;;80MB
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after init."
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold 800000) ;;800KB
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'focus-out-hook 'garbage-collect))))


;; Do not use garbage-collect when use minibuffer
;; @see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(defun eye-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun eye-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'eye-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'eye-minibuffer-exit-hook)

;;;; system version
(setq is-windows (or
		  (eq system-type 'windows-nt)
		  (eq system-type 'cygwin)))
(setq is-linux (eq system-type 'gnu/linux))
(setq is-mac (eq system-type 'darwin))

(setq is-gui (display-graphic-p))
(setq is-terminal (not (display-graphic-p)))

;;;; load custom-file before all init-* config
(setq custom-file (expand-file-name "custom-set-variables.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (shell-command (concat "touch " custom-file)))
(load custom-file t t)

;;;; startup time
(defvar startup-time (current-time))
(defvar begin-time nil "This is for calc require package time")
(defun eye-print-startup-time ()
  (message (format
	    "\n\nEmacs startup time: %.6f seconds.\n\n\n"
	    (- (float-time (current-time)) (float-time startup-time)))))

(defun eye--print-time (msg)
  "Print cost time from begin-time.
 use eye--reset-time to reset begin-time."
  (message (format
	    "require cost time %.6f sec: %s"
	    (- (float-time (current-time)) (float-time begin-time))
	    msg)))
(defun eye-reset-begin-time ()
  (setq begin-time (current-time)))

(defun eye-require (feature msg)
  (eye-reset-begin-time)
  (require feature)
  (eye--print-time msg))


;;(add-to-list 'load-path "~/src/emacs-packages/idle-require")
(require 'cl-lib)
;;(require 'idle-require)
;;(setq idle-require-idle-delay 0.5)
(defvar idle-load-features nil)
(cl-defmacro idle-load (feature &key req before after)
  "自动使用idle-require加载配置
需要先require cl-lib和 idle-require
比如加载apt-utils，require前的内容放在:before参数中，require后的配置放在:after中
req：t或nil，表示是否添加到idle-require中，在idle-require中的，启动后开始加载。nil则会根据按键调用，自动加载
(idle-load 'apt-utils
     :req t
     :before  (message \"idle-load:before finished\")
     :after   (message \"idle-load:after finished\"))
最后调用(idle-require-mode 1)进行加载
"
  `(progn ,before
	  (with-eval-after-load ,feature ,after)
	  ;;(if ,req (add-to-list 'idle-require-symbols ,feature t))
	  (if ,req (add-to-list 'idle-load-features ,feature t))
	  ))

(defun idle-load-startup ()
  (run-with-idle-timer 1 nil
		       #'(lambda ()
			   (dolist (feature idle-load-features)
			     ;;(message "idle load: %s" feature)
			     (require feature))
			   (message "Idle load finished!"))))
			   


;;;; server-mdoe
;; 使用 emacsclient 需要先启动服务
(idle-load 'server
	   :req t
	   :after
	   (if (not (equal t (server-running-p)))
	       (server-start)))

;;;; color theme
;;(load-theme 'wombat t)

;;;; keywords hightlight
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


;;;; font
(when is-linux
  (setq en-font-name "Inconsolata")
  (setq cn-font-name "文泉驿等宽微米黑")
  (setq en-font-size 14)
  (setq cn-font-size 12)
  )
(when is-windows
  ;; Inconsolata
  ;; Fira Code
  ;; Droid Sans Mono Wide
  (setq en-font-name "Inconsolata")
  (setq cn-font-name "WenQuanYi Micro Hei Mono")
  (setq en-font-size 14)
  (setq cn-font-size 12)
  )

;; 获取屏幕分辨率自动增大字体
(when (and is-gui
	   (> (x-display-pixel-width) 1366)
	   (> (x-display-pixel-height) 768))
  (setq en-font-size (+ en-font-size 2))
  (setq cn-font-size (+ cn-font-size 2)))


(defun eye-update-font-size ()
  ;; English font
  (set-face-attribute
   'default nil
   :font (font-spec :family en-font-name
                    :weight 'normal
                    :slant 'normal
                    :size en-font-size))
  ;; Chinese font
  (if is-gui
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
	(set-fontset-font
	 (frame-parameter nil 'font)
	 charset
	 (font-spec :family cn-font-name
                    :weight 'normal
                    :slant 'normal
                    :size cn-font-size))))
  )

(eye-update-font-size)

(defun eye/increase-font-size ()
  "Increase font size of english and chinese."
  (interactive)
  (setq en-font-size (+ en-font-size 1))
  (setq cn-font-size (+ cn-font-size 1))
  (eye-update-font-size)
  )

(defun eye/decrease-font-size ()
  "Decrease font size of english and chinese."
  (interactive)
  (setq en-font-size (- en-font-size 1))
  (setq cn-font-size (- cn-font-size 1))
  (eye-update-font-size)
  (if (equal (frame-parameter nil 'fullscreen) 'maximize)
      (maximize-frame))
  )

;;;; theme: modeline
;; Copy from https://github.com/redguardtoo/emacs.d/blob/master/lisp/init-modeline.el
;; @see http://emacs-fu.blogspot.com/2011/08/customizing-mode-line.html
;; But I need global-mode-string,
;; @see http://www.delorie.com/gnu/docs/elisp-manual-21/elisp_360.html
;; use setq-default to set it for /all/ modes
(defun set-header-line-format ()
  (setq-default header-line-format
		(list
		 ;;"%e"
		 ;;mode-line-front-space
		 " "
		 ;; the buffer name; the file name as a tool tip
		 '(:eval (propertize (if (buffer-modified-p)
					 "%b *"
                                       "%b")
                                     'face nil))
		 " "
		 ;; the current major mode for the buffer.
		 "["
		 '(:eval (propertize "%m" 'face nil))
		 " "
		 ;; insert vs overwrite mode, input-method in a tooltip
		 '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                                     'face nil
                                     'help-echo (concat "Buffer is in "
							(if overwrite-mode "overwrite" "insert") " mode")))
		 ;; is this buffer read-only?
		 '(:eval (when buffer-read-only
                           (concat ","  (propertize "RO" 'face nil))))
		 "] "    
		 '(:eval (format "%s" buffer-file-coding-system))
		 " "
		 "%n " ;; narrow state
		 ;; line and column
		 "" ;; '%02' to set to 2 chars at least; prevents flickering
		 "L%02l" ;; "," "%01c"
		 ;; (propertize "%02l" 'face 'font-lock-type-face) ","
		 ;; (propertize "%02c" 'face 'font-lock-type-face)
		 " "
		 ;;global-mode-string, org-timer-set-timer in org-mode need this
		 (propertize "%M" 'face nil)
		 )))
;; Show modeline information on top header
;; (setq header-line-format mode-line-format)
;; (setq-default header-line-format mode-line-format) ; Copy mode-line
;; Remove mode-line
;;(setq mode-line-format nil)
;;(setq-default mode-line-format nil)
;;(force-mode-line-update)
;;(set-face-attribute 'header-line nil :background "grey70" :foreground "purple4")

;; display the real names on mode-line when visiting a symbolink
(setq find-file-visit-truename t)


;;;; backup
(defvar user-cache-directory "~/tmp/emacs_cache")

(unless (file-directory-p "~/tmp")
  (make-directory "~/tmp"))
(unless (file-directory-p user-cache-directory)
  (make-directory user-cache-directory))
(unless (file-directory-p (concat user-cache-directory "/bak"))
  (make-directory (concat user-cache-directory "/bak")))
;; 备份文件 file~，指定备份目录后，文件名为 !drive_f!dirname!dirname!filename~
(setq backup-by-copying t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)
(setq backup-directory-alist '(("." . "~/tmp/emacs_cache/bak")))
;; 临时文件 #file#
(setq auto-save-file-name-transforms '((".*" "~/tmp/emacs_cache/bak" t))) ;; 设置备份文件目录
;; (setq auto-save-default t) ;; 是否开启自动备份临时文件，auto-save.el 中会修改这个变量
;; (setq auto-save-timeout 5)
(setq delete-by-moving-to-trash t)	;删除文件或目录时，移到回收站

;;;; auto revert
(global-auto-revert-mode 1)


;;;; undo
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

;;;; misc
;; 防止退出时卡死在 Saving clipboard to X clipboard manager 状态
(setq x-select-enable-clipboard-manager nil)

(setq inhibit-startup-message t) ;; 禁用启动后显示的消息 buffer
(setq initial-scratch-message nil) ;; 禁止显示 *scratch* buffer 中默认出现的文本
(setq inhibit-compacting-font-caches t) ;; 防止卡顿，Don’t compact font caches during GC.
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-screen t)
(setq use-file-dialog nil)
(setq use-dialog-box nil)

(put 'suspend-frame 'disabled t) ;; 禁止 Ctrl+z 时挂起 emacs

;; 用 y/n 代替 yes/no 的回答
(defalias 'yes-or-no-p 'y-or-n-p) ;; (fset 'yes-or-no-p 'y-or-n-p) 相同的效果

(setq ring-bell-function 'ignore) ;; 禁止出现烦人的响铃

(setq truncate-lines t) ;; 不自动折行

;; Fix load slow, https://github.com/raxod502/radian/issues/180
(when tool-bar-mode (tool-bar-mode -1)) ;; 禁用工具栏
(when menu-bar-mode (menu-bar-mode -1)) ;; 禁用菜单栏
(when (and is-gui scroll-bar-mode)
  (scroll-bar-mode -1)) ;; 禁用滚动条 emacs26 -nw will be error

(setq frame-title-format "Editor") ;; 自定义标题栏

;; 去掉窗口边缘和分割窗口时分割条的边缘
;; http://emacsredux.com/blog/2015/01/18/customizing-the-fringes/
;; linux maybe need gdb, and use mouse to set breakpoint on fringe, so only hide fringe on windows.
(when (and is-windows is-gui)
  (set-window-fringes nil 10 0) ;; border side
  (fringe-mode '(10 . 0)) ;; middle of split frame
  )

(when is-gui
  (blink-cursor-mode -1) ;; 取消光标闪烁
  (add-hook 'after-init-hook
	    (lambda ()
	      (set-cursor-color "#00A876"))))

(setq mouse-yank-at-point t) ;; 强制粘贴时粘贴到光标处

;; @see https://stackoverflow.com/questions/2081577/setting-emacs-split-to-horizontal
;; (setq split-width-threshold nil) ;;不允许自动左右分屏
(setq split-height-threshold nil) ;;不允许自动上下分屏

;; 不要自动分割窗口 @see https://github.com/ecxr/handmadehero/blob/master/misc/.emacs
;; (setq split-window-preferred-function nil)

(setq mouse-wheel-scroll-amount '(2 ((shift) . 2))) ;; 鼠标滚轮滑动一次滚动多少行
;; (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;; (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;; (setq scroll-step 5) ;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)


;; (setq electric-pair-pairs '((?\{ . ?\})
                            ;; (?\( . ?\))
                            ;; (?\[ . ?\])
                            ;; (?\" . ?\")))
;; (electric-pair-mode t) ;;自动输出成对括号

(show-paren-mode 1) ;;高亮匹配的括号



;; enable narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; save clipboard contents into kill-ring before replace theme
(setq save-interprogram-paste-before-kill t)

(delete-selection-mode 1)

;; Kill buffers without asking
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))


(setq ibuffer-expert t) ;;don't ask when delete

;; Don't open a file in a new frame
(when (boundp 'ns-pop-up-frames)
  (setq ns-pop-up-frames nil))

;;(setq track-eol t) ;; 保持光标上下移动时一直在行尾，需要设置line-move-visual为nil
;; (setq line-move-visual t)		;在长行中移动
(global-visual-line-mode 1)


;; 最大化
(defun maximize-frame ()
  "Maximizes the active frame in Windows"
  (interactive)
  (set-frame-parameter nil 'fullscreen 'maximize)
  ;; Send a `WM_SYSCOMMAND' message to the active frame with the
  ;; `SC_MAXIMIZE' parameter.
  (if is-windows
      (w32-send-sys-command 61488)))

(defun fullscreen-toggle ()
  "Toggle fullscreen/maximize status."
  (interactive)
  (if (equal 'fullboth (frame-parameter nil 'fullscreen))
      (maximize-frame)
    (set-frame-parameter nil 'fullscreen 'fullboth)))


(when is-gui
    (add-hook 'after-init-hook 'maximize-frame))


;;;; load-path
(idle-load 'init-packages :req t :before (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))

;;;; fly keys
;; disable all mouse key
(dolist (k '([mouse-1] [down-mouse-1] [drag-mouse-1] [double-mouse-1] [triple-mouse-1]  
             [mouse-2] [down-mouse-2] [drag-mouse-2] [double-mouse-2] [triple-mouse-2]
             [mouse-3] [down-mouse-3] [drag-mouse-3] [double-mouse-3] [triple-mouse-3]
             [mouse-4] [down-mouse-4] [drag-mouse-4] [double-mouse-4] [triple-mouse-4]
             [mouse-5] [down-mouse-5] [drag-mouse-5] [double-mouse-5] [triple-mouse-5]))
  (global-unset-key k))

(idle-load 'init-leader-key :req t)

;;;; which-key
(idle-load 'which-key
	   :req t
	   :before
	   (add-to-list 'load-path "~/src/emacs-packages/emacs-which-key")
	   :after
	   (which-key-mode 1))

;;;; saveplace
(idle-load 'saveplace
	   :req t
	   :after (setq save-place-file "~/tmp/emacs_cache/places"))

;;;; recentf
(idle-load 'recentf
	   :req t
	   :after
	   (progn
	     (setq recentf-max-saved-items 100)
	     ;;(add-to-list 'recentf-exclude (expand-file-name package-user-dir))
	     (add-to-list 'recentf-exclude ".cache")
	     (add-to-list 'recentf-exclude ".cask")
	     (add-to-list 'recentf-exclude "ido.last")
	     (add-to-list 'recentf-exclude "bookmarks")
	     (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
	     ))

(require 'lisp-mode
	 :after
	 (progn
	   (defun setup-lisp-mode ()
	     (setq tab-with 2))
	   (add-hook 'emacs-lisp-mode-hook 'setup-lisp-mode)))

;;;; savehist: save minibuffer history
(idle-load 'savehist
	   :req t
	   :after
	   (progn
	     (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
		   history-length 100
		   savehist-autosave-interval nil ;;不开启自动保存，否则会不断的分配内存
		   savehist-additional-variables '(mark-ring global-mark-ring search-ring regexp-search-ring extended-command-history))
	     ))


;; for quick startup
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (save-place-mode 1)
	    (recentf-mode 1)
	    (savehist-mode 1)))

;;;; helm
(idle-load 'helm-mode
	   :before
	   (progn
	     (add-to-list 'load-path "~/src/emacs-packages/emacs-async")
	     (add-to-list 'load-path "~/src/emacs-packages/helm"))
	   :after
	   (progn
	     (setq helm-autoresize-max-height 8
		   helm-autoresize-min-height 8
		   helm-display-buffer-default-height 8
		   helm-display-buffer-height 8
		   helm-M-x-fuzzy-match t
		   helm-buffers-fuzzy-matching t
		   helm-recentf-fuzzy-match t
		   helm-semantic-fuzzy-match t
		   helm-imenu-fuzzy-match t
		   helm-split-window-in-side-p nil
		   helm-move-to-line-cycle-in-source nil
		   helm-ff-search-library-in-sexp t
		   helm-scroll-amount 8 
		   helm-echo-input-in-header-line nil)
	     (helm-mode 1)
	     ))


;;;; helm-dash
;; must install sqlite3
(idle-load 'helm-dash
	   :before
	   (progn
	     (add-to-list 'load-path "~/src/emacs-packages/helm")
	     (add-to-list 'load-path "~/src/emacs-packages/helm-dash"))
	   :after
	   (progn
	     (setq helm-dash-docsets-path "~/.docsets"
		   ;; helm-dash-common-docsets '("CMake")
		   helm-dash-min-length 3
		   helm-dash-browser-func 'browse-url-generic) ;; or 'eww

	     (setq browse-url-browser-function 'browse-url-generic
		   browse-url-generic-program (if is-windows
						  "C:/Program Files (x86)/Maxthon5/Bin/Maxthon.exe"
						"/usr/bin/firefox"
						))
	     (defun emacs-lisp-dash ()
	       (interactive)
	       (setq-local helm-dash-docsets '("Emacs Lisp")))
	     (add-hook 'emacs-lisp-mode-hook 'emacs-lisp-dash)
	     (add-hook 'lisp-interaction-mode-hook 'emacs-lisp-dash)
	     ))


;;;; load packages
(defvar is-enable-posframe nil)
(idle-load 'base-toolkit  :before (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))
;; (idle-load 'init-hydra    :before (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))
;; (idle-load 'init-keys     :before (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))
(idle-load 'init-locale   :req t :before (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))
(idle-load 'init-orgmode  :before (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))
      

;;;; encodings
(idle-load 'mule
	   :req t
	   :after
	   (progn
	     (setq locale-coding-system 'utf-8)     ;; 设置emacs 使用 utf-8
	     (set-language-environment 'Chinese-GB) ;; 设置为中文简体语言环境
	     (set-keyboard-coding-system 'utf-8)    ;; 设置键盘输入时的字符编码
	     ;; 解决粘贴中文出现乱码的问题
	     (if (eq system-type 'windows-nt)
		 (progn
		   ;; (setq selection-coding-system 'utf-16le-dos) ;; 修复从网页剪切文本过来时显示 \nnn \nnn 的问题
		   ;; (set-default selection-coding-system 'utf-16le-dos)
		   (set-selection-coding-system 'utf-16le-dos) ;; 别名set-clipboard-coding-system
		   )
	       (set-selection-coding-system 'utf-8))
	     (prefer-coding-system 'utf-8)
	     ;; 文件默认保存为 utf-8
	     (set-buffer-file-coding-system 'utf-8)
	     (set-default buffer-file-coding-system 'utf8)
	     (set-default-coding-systems 'utf-8)
	     ;; 防止终端中文乱码
	     (set-terminal-coding-system 'utf-8)
	     (modify-coding-system-alist 'process "*" 'utf-8)
	     (setq default-process-coding-system '(utf-8 . utf-8))
	     ;; 解决文件目录的中文名乱码
	     (setq-default pathname-coding-system 'utf-8)
	     (set-file-name-coding-system 'utf-8)

	     (when is-windows
	       (setq default-process-coding-system '(gbk . gbk))
	       ;; file encoding
	       ;; @see https://www.gnu.org/software/emacs/manual/html_node/emacs/Recognize-Coding.html
	       (modify-coding-system-alist 'file "\\.txt\\'" 'chinese-iso-8bit-dos)
	       (modify-coding-system-alist 'file "\\.h\\'" 'chinese-iso-8bit-dos)
	       (modify-coding-system-alist 'file "\\.cpp\\'" 'chinese-iso-8bit-dos)
	       )

	     ;; windows shell
	     (when (and is-windows is-terminal)
	       (defun eye/change-shell-mode-coding ()
		 (progn
		   (set-terminal-coding-system 'gbk)
		   (set-keyboard-coding-system 'gbk)
		   ;; (set-selection-coding-system 'gbk)
		   (set-buffer-file-coding-system 'gbk)
		   (set-file-name-coding-system 'gbk)
		   (modify-coding-system-alist 'process "*" 'gbk)
		   (set-buffer-process-coding-system 'gbk 'gbk)
		   ))
	       (add-hook 'shell-mode-hook 'eye/change-shell-mode-coding)
	       (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
	       (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))
	     ))

(idle-load 'bookmark
	   :req nil
	   :after
	   (progn
	     ;; 自动保存书签
	     (add-hook 'kill-emacs-hook
		       '(lambda ()
			  (bookmark-save)))
	     ))

(idle-load 'ido
	   :req t
	   :after
	   (progn
	     (ido-mode t)
	     (setq ido-enable-flex-matching t) ;; enable fuzzy matching
	     (setq ido-auto-merge-delay-time 10000) ;; disable auto search file
	     ))

(idle-load 'smex
	   :req t
	   :before (add-to-list 'load-path "~/src/emacs-packages/smex")
	   :after
	   (progn
	     ;; modify smex so that typing a space will insert a hyphen ‘-’ like in normal M-x
	     ;; @see https://www.emacswiki.org/emacs/Smex
	     (defadvice smex (around space-inserts-hyphen activate compile)
               (let ((ido-cannot-complete-command 
		      `(lambda ()
			 (interactive)
			 (if (string= " " (this-command-keys))
			     (insert ?-)
			   (funcall ,ido-cannot-complete-command)))))
		 ad-do-it))
	     ))

(idle-load 'imenu
	   :after
	   (progn
	     (add-to-list 'imenu-generic-expression '("sections" "^;;;; \\(.+\\)$" 1) t)
	     (setq imenu-auto-rescan t
		   imenu-auto-rescan-maxout 500000)))

(idle-load 'dired
	   :req nil
	   :before
	   (progn
	     (add-to-list 'load-path "~/src/emacs-packages/emacs-async")
	     (add-to-list 'load-path "~/src/emacs-packages/w32-browser")
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
	     ;; 隐藏 dired 中文件拥有者和文件权限等信息
	     (defun eye-dired-mode-setup ()
	       "hide the file's unix owner and permission info"
	       (dired-hide-details-mode 1)		;隐藏以.开头的文件
	       (dired-omit-mode 1)			;隐藏.和..本身 @see https://stackoverflow.com/questions/43628315/how-to-hide-one-dot-current-directory-in-dired-mode
	       )
	     (add-hook 'dired-mode-hook 'eye-dired-mode-setup)
	     )
	     
	   :after
	   (progn
	     (require 'wdired)
	     (require 'dired-x) ;; 支持 dired-jump 进入后自动定位到当前文件名位置
	     (require 'async) ;; 目录有变化时及时更新
	     (setq dired-async-mode 1)

	     ;; 打开 .dired 后缀文件时，自动进入 dired-virtual-mode 模式。
	     (setq auto-mode-alist (cons '("[^/]\\.dired$" . dired-virtual-mode)
					 auto-mode-alist))

	     (add-hook 'imenu-after-jump-hook (lambda () (recenter 0)))
	     
	     (define-key dired-mode-map (kbd "<f12>s") 'dired-dotfiles-toggle)

	     ;; 使用 windows 程序打开文件
	     ;;	(when is-windows
	     ;;	 (require 'w32-browser))
	     ))

;;;; org note
(idle-load 'init-org-note)

;;;; keyfreq
(idle-load 'keyfreq
	   :req t
	   :before
	   (add-to-list 'load-path "~/src/emacs-packages/keyfreq")
	   :after
	   (progn
	     (keyfreq-mode 1)
	     (keyfreq-autosave-mode 1)))

;;;; tramp
(idle-load 'tramp
	   :after
	   (progn
	     (setq password-cache-expiry 360000000      ;设置密码过期时间，避免每次询问密码
		   tramp-default-method "ssh")
	     ))


;;;; site packages
(idle-load 'eno
	   :before
	   (progn
	     (add-to-list 'load-path "~/src/emacs-packages/edit-at-point")
	     (add-to-list 'load-path "~/src/emacs-packages/dash")
	     (add-to-list 'load-path "~/src/emacs-packages/eno"))
	   :after
	   (progn
	     (defun eye/eno-copy ()
	       (interactive)
	       (cond
		((equal major-mode 'c++-mode)
		 (eno-word-copy))
		((or (equal major-mode 'emacs-lisp-mode) (equal major-mode 'lisp-interaction-mode))
		 (eno-symbol-copy))
		(t (eno-word-copy))))
	     ))
;;(when is-gui (eye-require 'init-theme "theme"))

;;;; all-the-icons
(idle-load 'all-the-icons
	   :req t
	   :before
	   (progn
	     (unless (or is-windows (member "all-the-icons" (font-family-list)))
	       (all-the-icons-install-fonts t)))
	   :after
	   (progn
	     (add-to-list 'all-the-icons-icon-alist '("\\.ipynb" all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
	     (add-to-list 'all-the-icons-mode-icon-alist '(ein:notebooklist-mode all-the-icons-faicon "book" :face all-the-icons-orange))
	     (add-to-list 'all-the-icons-mode-icon-alist '(ein:notebook-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
	     (add-to-list 'all-the-icons-mode-icon-alist '(ein:notebook-multilang-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
	     (add-to-list 'all-the-icons-icon-alist '("\\.epub$" all-the-icons-faicon "book" :face all-the-icons-green))
	     (add-to-list 'all-the-icons-mode-icon-alist '(nov-mode all-the-icons-faicon "book" :face all-the-icons-green))
	     (add-to-list 'all-the-icons-mode-icon-alist '(gfm-mode  all-the-icons-octicon "markdown" :face all-the-icons-lblue))))

;;;; solaire-mode
(idle-load 'solaire-mode
	   :req t
	   :after
	   (progn
	     ;; Enable solaire-mode anywhere it can be enabled
	     (solaire-global-mode +1)
	     ;; To enable solaire-mode unconditionally for certain modes:
	     (add-hook 'ediff-prepare-buffer-hook #'solaire-mode)

	     ;; ...if you use auto-revert-mode, this prevents solaire-mode from turning
	     ;; itself off every time Emacs reverts the file
	     (add-hook 'after-revert-hook #'turn-on-solaire-mode)

	     ;; highlight the minibuffer when it is activated:
	     (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)

	     ;; if the bright and dark background colors are the wrong way around, use this
	     ;; to switch the backgrounds of the `default` and `solaire-default-face` faces.
	     ;; This should be used *after* you load the active theme!
	     ;;
	     ;; NOTE: This is necessary for themes in the doom-themes package!
	     (solaire-mode-swap-bg)))

;;;; doom-theme
(when is-gui (idle-load 'doom-themes
			:req t
			:after
			(progn
			  ;; Enable flashing mode-line on errors
			  (doom-themes-visual-bell-config)
			  ;; Corrects (and improves) org-mode's native fontification.
			  (doom-themes-org-config)
			  (load-theme 'doom-one t)
			  )))

;;;; doom modeline
(idle-load 'doom-modeline
	   :req t
	   :before
	   (progn
	     (setq doom-modeline-major-mode-color-icon t
		   doom-modeline-github t
		   ;;How tall the mode-line should be (only respected in GUI Emacs).
		   doom-modeline-height 25
		   ;;How wide the mode-line bar should be (only respected in GUI Emacs).
		   doom-modeline-bar-width 3
		   ;;don't use github notifications, because must set github.user, if not, will has a lot of emacs error process
		   doom-modeline-github nil
		   ))
	   :after
	   (progn
	     (doom-modeline-mode 1)
	     ;;(cancel-timer doom-modeline--github-timer)
	     ))

;;;; time
;(require 'time)
;(setq display-time-24hr-format t)
;(setq display-time-day-and-date nil)
;(display-time-mode)

;; (when is-terminal
  ;; (set-face-attribute 'hl-line nil :background "darkgray"))

(idle-load 'avy)

(idle-load 'avy-zap)

(idle-load 'ace-jump)

(idle-load 'init-idomenu)

(idle-load 'init-ivy)

(idle-load 'super-save :req t
	   :after
	   (progn
	     (setq super-save-remote-files nil)
	     (super-save-mode 1)))

(idle-load 'symbol-overlay)

;;;; bm
(idle-load 'bm
	   :before
	   (setq bm-cycle-all-buffers nil		;; 是否在所有buffer中循环
		 ;; (setq bm-in-lifo-order t)		;; 先入先出
		 bm-restore-repository-on-load t
		 ;; where to store persistant files
		 bm-repository-file "~/.emacs.d/bm-repository"
		 ;; save bookmarks
		 bm-buffer-persistence t)
	   :after
	   (progn
	     ;; Loading the repository from file when on start up.
	     (bm-repository-load)
	     ;; Saving bookmarks
	     (add-hook 'kill-buffer-hook #'bm-buffer-save)

	     ;; Saving the repository to file when on exit.
	     ;; kill-buffer-hook is not called when Emacs is killed, so we
	     ;; must save all bookmarks first.
	     (add-hook 'kill-emacs-hook #'(lambda nil
					    (bm-buffer-save-all)
					    (bm-repository-save)))

	     ;; The `after-save-hook' is not necessary to use to achieve persistence,
	     ;; but it makes the bookmark data in repository more in sync with the file
	     ;; state.
	     (add-hook 'after-save-hook #'bm-buffer-save)

	     ;; Restoring bookmarks
	     (add-hook 'find-file-hooks   #'bm-buffer-restore)
	     (add-hook 'after-revert-hook #'bm-buffer-restore)

	     ;; The `after-revert-hook' is not necessary to use to achieve persistence,
	     ;; but it makes the bookmark data in repository more in sync with the file
	     ;; state. This hook might cause trouble when using packages
	     ;; that automatically reverts the buffer (like vc after a check-in).
	     ;; This can easily be avoided if the package provides a hook that is
	     ;; called before the buffer is reverted (like `vc-before-checkin-hook').
	     ;; Then new bookmarks can be saved before the buffer is reverted.
	     ;; Make sure bookmarks is saved before check-in (and revert-buffer)
	     (add-hook 'vc-before-checkin-hook #'bm-buffer-save)

	     ;; 设置样式
	     (set-face-attribute 'bm-persistent-face nil :foreground "#ff7800" :background "#1142AA")
	     (require 'ext-bm)
	     ))	   

(idle-load 'ibuffer
	   :before
	   (progn
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
		      ))))
	   :after
	   (progn
	     (add-hook 'ibuffer-mode-hook
		       '(lambda ()
			  (ibuffer-auto-mode 1)
			  (ibuffer-switch-to-saved-filter-groups "EL")))
	     (setq ibuffer-show-empty-filter-groups nil)
	     ))

(idle-load 'init-hungry-delete :req t :after (global-hungry-delete-mode 1))
(idle-load 'rainbow-mode)

(idle-load 'aweshell)
(idle-load 'init-web-search)

(idle-load 'watch-other-window
	   :before
	   (progn
	     (autoload 'watch-other-window-up "watch-other-window" "" t)
	     (autoload 'watch-other-window-down "watch-other-window" "" t)
	     (autoload 'watch-other-window-up-line "watch-other-window" "" t)
	     (autoload 'watch-other-window-down-line "watch-other-window" "" t)))

(idle-load 'rg)
(idle-load 'color-rg)

(idle-load 'init-elisp :req t)

;;;; orgmode
(idle-load 'init-orgmode :req t)

;;;; yasnippet
(idle-load 'yasnippet)

;;;; ivy-yasnippet
(idle-load 'ivy-yasnippet)

;;;; yankpad
(idle-load 'yankpad
	   :after
	   (progn
	     ;; ~/.emacs.d/snippets is yas--default-user-snippets-dir
	     ;;如果手动更换orgmode9后，这句执行后出现Not a face: nil的奇怪问题，终端下ivy无法弹出来，如果是赋值为不带/的字符串，又不会出现问题
	     (setq yankpad-file (expand-file-name "yankpad.org" locale-notebook-dir))
	     ;; (add-to-list 'hippie-expand-try-functions-list #'yankpad-expand)
	     ))


;;;; cc-mode
(idle-load 'cc-mode
	   :before
	   (require 'init-cpp)
	   :after
	   (progn
	     (add-hook 'c++-mode-hook #'eye-setup-c++)
	     (define-key c++-mode-map (kbd ",") nil)))

;;;; whitespace
;; http://ergoemacs.org/emacs/whitespace-mode.html
(idle-load 'whitespace
	   :before
	   (progn
	     ;; Make whitespace-mode with very basic background coloring for whitespaces.
	     ;; http://ergoemacs.org/emacs/whitespace-mode.html
	     (setq whitespace-style (quote (face spaces tabs newline space-mark tab-mark newline-mark )))

	     ;; Make whitespace-mode and whitespace-newline-mode use “¶” for end of line char and “▷” for tab.
	     (setq whitespace-display-mappings
		   ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
		   '(
		     (space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
		     (newline-mark 10 [182 10]) ; LINE FEED,
		     (tab-mark 9 [9655 9] [92 9]) ; tab
		     ))))


;;;; company-c-headers
(idle-load 'company-c-headers
	   :after
	   (progn
	     (add-hook 'c++-mode-hook (lambda () (add-to-list 'company-backends 'company-c-headers)))
	     (when is-windows
	       (setq company-c-headers-path-system '("C:/Program Files (x86)/Microsoft Visual Studio 14.0/VC/include"
						     "C:/Program Files (x86)/Microsoft SDKs/Windows\v7.1A/Include")))))


;;;; php-mode
(idle-load 'php-mode
	   :before
	   (progn
	     (add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
	     (add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))))

;;;; qt-pro-mode
(defun setup-qt-pro-mode ()
  (interactive)
  ;; qt keywords and stuff ...
  ;; set up indenting correctly for new qt kewords
  (setq c-protection-key (concat "\\<\\(public\\|public slot\\|protected"
				 "\\|protected slot\\|private\\|private slot"
				 "\\)\\>")
	c-C++-access-key (concat "\\<\\(signals\\|public\\|protected\\|private"
				 "\\|public slots\\|protected slots\\|private slots"
				 "\\)\\>[ \t]*:"))
  (progn
    ;; modify the colour of slots to match public, private, etc ...
    (font-lock-add-keywords 'c++-mode
			    '(("\\<\\(slots\\|signals\\)\\>" . font-lock-type-face)))
    ;; make new font for rest of qt keywords
    (make-face 'qt-keywords-face)
    (set-face-foreground 'qt-keywords-face "DeepSkyBlue1")
    ;; qt keywords
    (font-lock-add-keywords 'c++-mode
			    '(("\\<Q_OBJECT\\>" . 'qt-keywords-face)))
    (font-lock-add-keywords 'c++-mode
			    '(("\\<SIGNAL\\|SLOT\\>" . 'qt-keywords-face)))
    (font-lock-add-keywords 'c++-mode
			    '(("\\<Q[A-Z][A-Za-z]\\>" . 'qt-keywords-face)))
    )
  (defun eye/qt5-help ()
    "Find Qt5 document."
    (interactive)
    (let ((url "http://doc.qt.io/qt-5/search-results.html?q="))
      (setq url (concat url (read-string "Query Qt5 document: " (eye/current-word))))
      (browse-url-firefox url))))

;;;; qt-pro-mode
(idle-load 'qt-pro-mode
	   :before
	   (progn
	     (add-to-list 'auto-mode-alist '("\\.pro$" . qt-pro-mode))
	     (add-to-list 'auto-mode-alist '("\\.pri$" . qt-pro-mode))     
	     (add-hook 'qt-pro-mode-hook 'setup-qt-pro-mode)))

;;;; css-mode
(idle-load 'css-mode
	   :before
	   (add-to-list 'auto-mode-alist '("\\.qss$" . css-mode)))

;;;; qml-mode
(idle-load 'qml-mode
	   :before
	   (progn
	     (autoload 'qml-mode "qml-mode" "Editing Qt Declarative." t)
	     (add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))))

;;;; ctags
(idle-load 'init-ctags)
(idle-load 'init-counsel-etags)

;;;; company
(idle-load 'init-company)

;;;; company-quickhelp
(idle-load 'company-quickhelp
	   :before
	   ;;company-quickhelp-manual-begin is not autoload function, must define.
	   (autoload 'company-quickhelp-manual-begin "company-quickhelp" "quickhelp" t)
	   :after
	   (progn
	     ;; 手动触发显示
	     (setq company-quickhelp-delay nil)
	     (define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin)))

;;;; ffit
(idle-load 'find-file-in-tags)

;;;; ffip
(idle-load 'find-file-in-project
	   :before
	   (progn
	     ;; Windows平台必须设置，否则执行ffip会直占用CPU。
	     (when is-windows (setq ffip-find-executable "find"))
	     (setq ffip-find-executable "find")))

;;;; awesome-tab
(idle-load 'awesome-tab :req t
	   :before
	   (progn
	     (setq awesome-tab-style 'zigzag))
	   :after
	   (progn
	     (awesome-tab-mode 1)
	     (define-key global-map (kbd "<C-tab>") #'awesome-tab-forward-tab)
	     (define-key global-map (kbd "<C-S-tab>") #'awesome-tab-backward-tab)))

;;;; awesome-tray
(idle-load 'awesome-tray
	   :req nil
	   :after
	   (progn
	     (awesome-tray-mode 1)))

;;;; global readonly mode
(idle-load 'global-readonly-mode)

(idle-load 'init-external)

(when is-linux
  (idle-load 'init-magit)
  (idle-load 'apt-utils)
  (idle-load 'init-snails))


(idle-load-startup)
;;(idle-require-mode 1)


(provide 'configuration)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
