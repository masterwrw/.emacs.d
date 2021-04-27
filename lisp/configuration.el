;;; configuration.el --- My emacs configuration -*- lexical-binding: t -*-

;;;; debug
;;Produce backtraces when errors occur
(setq debug-on-error nil)

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
(setq custom-file (expand-file-name (if is-windows
					"custom-set-variables-win.el"
				      "custom-set-variables-linux.el"
				      user-emacs-directory)))
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
(add-hook 'after-init-hook #'eye-print-startup-time)

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


(require 'cl-lib)
(defvar auto-require-packages-dir "~/.emacs.d/packages")

(defun add-package-path (dirlist)
  (cond ((stringp dirlist)
	 (add-to-list 'load-path (concat auto-require-packages-dir "/" dirlist)))
	((listp dirlist)
	 (dolist (dir dirlist)
	   (add-to-list 'load-path (concat auto-require-packages-dir "/" dir))))
	(t (error "Wrong arg for add-package-path"))))

(defun add-autoload (functions file)
  (let ((fv (if (stringp file)
		file (car file))))
    (if (listp functions)  ;; functions是列表时
	(dolist (fun functions)
	  (if (listp fun)   ;; 支持 '((helm-find-files . "helm-find")) 的形式
	      (autoload (car fun) (cdr fun) "" t)
	    (autoload fun fv "" t)))  ;; functions是列表，且列表中直接是函数时，如 '(foo bar)
      (autoload functions fv "" t))))  ;; functions直接是 'foo 的形式

(cl-defmacro auto-require (feature &key load reqby paths functions before after)
  "自动加载插件
由于自行生成的autoload.pkg.el文件过大，加载时间变长，在这个宏中调用autoload，删除不用的插件时，autoload也就删除了。不会影响启动速度。

示例：(auto-require 'idle-require
;;                       :load t                       ;;load是指是否立即require，可以是t或nil，有reqby时，会等到指定的插件加载后才require
;;                       :reqby 'dash                  ;;dep是指需要哪个插件先加载，目前只支持写一个
;;                       :paths \"idle-require\"       ;;paths是指要添加的load-path的路径，
;;                       :functions 'idle-require-mode ;;functions是指要使用autoload的函数，autoload的file参数使用paths中的第一个，支持单独设置函数所在的file
;;                                                     ;;如：'((foo . \"foo\") (bar . \"bar\"))，此时不使用paths作为autoload的file参数
;;                       :before (setq somevar t)      ;; before是指require前的配置
;;                       :after (progn ....))          ;;after是指require后的配置

paths只需要设置插件存放的目录名，统一在auto-require-packages-dir下，可以是单个字符串，或者字符串列表
如果有functions参数，第一个字符串需要是插件存放的目录名，才能正确autoload
如果没有指定paths，就不要指定functions参数。
"
  `(progn
     (when ,paths
       (add-package-path ,paths)
       (if ,functions
	   (add-autoload ,functions ,paths)))
     ,before
     (if ,reqby
	 (with-eval-after-load ,reqby
	   (when ,load (require ,feature))
	   (with-eval-after-load ,feature ,after))
       (progn
	 (when ,load (require ,feature))
	 (with-eval-after-load ,feature ,after)))))

(defvar idle-load-features nil "用于启动后空闲加载的列表")
(defun idle-load-startup ()
  "放到配置最后"
  (run-with-idle-timer 1 nil
		       #'(lambda ()
			   (dolist (feature idle-load-features)
			     (require feature))
			   (message "Idle load finished!"))))


;;;; server-mdoe
;; 使用 emacsclient 需要先启动服务
(auto-require 'server
	      :after
	      (if (not (equal t (server-running-p)))
		  (server-start)))

;;;; color theme
;;(load-theme 'wombat t)

;;;; keywords hightlight
(when is-gui
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
  (modify-face 'font-lock-note-face "#33aa00" nil nil t nil t nil nil))


;;;; font
(if is-windows (setq cn-font-name "Microsoft YaHei") (setq cn-font-name "Noto Sans CJK SC Regular"))
(setq en-font-name "Source Code Pro")
(setq en-font-size 14 cn-font-size 14)

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

(when is-gui (eye-update-font-size))



;;;; theme: modeline
;; Copy from https://github.com/redguardtoo/emacs.d/blob/master/lisp/init-modeline.el
;; @see http://emacs-fu.blogspot.com/2011/08/customizing-mode-line.html
;; But I need global-mode-string,
;; @see http://www.delorie.com/gnu/docs/elisp-manual-21/elisp_360.html
;; use setq-default to set it for /all/ modes
(defun set-mode-line-format ()
  (setq-default mode-line-format
		(list
		 ;;"%e"
		 ;;mode-line-front-space
		 " "
		 ;; the buffer name; the file name as a tool tip
		 '(:eval (propertize (if (buffer-modified-p)
					 "%b *"
                                       "%b")
				     'face nil))
		 ;; 显示完整路径
		 ;;'(:eval (concat
		 ;;  (or (buffer-file-name) (buffer-name))
		 ;; (if (buffer-modified-p) " *")))
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

(set-mode-line-format)
;; Remove mode-line
;;(setq mode-line-format nil)
;;(setq-default mode-line-format nil)
;;(set-face-attribute 'header-line nil :background "grey70" :foreground "purple4")

(set-face-attribute 'mode-line nil :background "grey70" :foreground "purple4")
(force-mode-line-update)



;;;; time
;; 显示在modeline上
(require 'time)
(setq display-time-24hr-format t) ;; 显示为24小时格式
(setq display-time-day-and-date t) ;; 是否显示日期
(setq display-time-format "%Y-%m-%d %H:%M") ;; 自定义日期格式
(display-time-mode t)

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
;;(when menu-bar-mode (menu-bar-mode -1)) ;; 禁用菜单栏

;; 禁用滚动条 if no check is-gui, emacs26 -nw will be error
(when (and is-gui scroll-bar-mode)
  (scroll-bar-mode -1))

(setq frame-title-format "GNUEmacs") ;; 自定义标题栏

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
	      (set-cursor-color "#FF3300"))))

(setq mouse-yank-at-point t) ;; 强制粘贴时粘贴到光标处

;; @see https://stackoverflow.com/questions/2081577/setting-emacs-split-to-horizontal
;;(setq split-width-threshold nil) ;;不允许自动左右分屏
(setq split-height-threshold nil) ;;不允许自动上下分屏

;; 不要自动分割窗口 @see https://github.com/ecxr/handmadehero/blob/master/misc/.emacs
;; (setq split-window-preferred-function nil)

(setq mouse-wheel-scroll-amount '(2 ((shift) . 2))) ;; 鼠标滚轮滑动一次滚动多少行
;; (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;; (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;; (setq scroll-step 5) ;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;; 半屏滚动
(defun scroll-half-page-down ()
  "scroll down half the page"
  (interactive)
  (scroll-down (/ (window-body-height) 2)))

(defun scroll-half-page-up ()
  "scroll up half the page"
  (interactive)
  (scroll-up (/ (window-body-height) 2)))

(global-set-key "\M-n" 'scroll-half-page-up)
(global-set-key "\M-p" 'scroll-half-page-down)

(setq electric-pair-pairs '((?\{ . ?\})
							(?\( . ?\))
							(?\[ . ?\])
							(?\" . ?\")))
(electric-pair-mode t) ;;自动输出成对括号

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
  (set-frame-parameter nil 'fullscreen 'maximized)
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
;;(auto-require 'init-packages :load t :before (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;;(require 'init-packages)

;;;; hydra
(auto-require 'hydra	      
	      :load t
	      :paths "hydra")


;;;; leader-key
(auto-require 'init-leader-key
	      :load t
	      :reqby 'hydra)

;;;; which-key
(auto-require 'which-key
	      :load t
	      :paths "emacs-which-key"
	      :after
	      (progn
		(which-key-mode 1)
		(which-key-setup-side-window-bottom)))

;;;; saveplace
(auto-require 'saveplace
	      :load t
	      :after (setq save-place-file "~/tmp/emacs_cache/places"))

;;;; recentf
(auto-require 'recentf
	      ;;:load t
	      :after
	      (progn
		(setq recentf-max-saved-items 100)
		(setq recentf-save-file "~/tmp/emacs_cache/recentf") ;;使双系统切换后不清空记录
		;;(add-to-list 'recentf-exclude (expand-file-name package-user-dir))
		(add-to-list 'recentf-exclude ".cache")
		(add-to-list 'recentf-exclude ".cask")
		(add-to-list 'recentf-exclude "ido.last")
		(add-to-list 'recentf-exclude "bookmarks")
		(add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
		))

;;;; savehist
;; save minibuffer history
(auto-require 'savehist
	      :load t
	      :after
	      (progn
		(setq enable-recursive-minibuffers t ; Allow commands in minibuffers
		      history-length 100
		      savehist-autosave-interval nil ;;不开启自动保存，否则会不断的分配内存
		      savehist-additional-variables '(mark-ring global-mark-ring search-ring regexp-search-ring extended-command-history))
		))

;;;; windmove
(auto-require 'windmove)

;;;; helm
(auto-require 'helm-mode
	      :paths '("helm" "emacs-async")
	      :functions '((helm-find-files . "helm-find")
			   (helm-M-x . "helm-command")
			   (helm-recentf . "helm-for-files")
			   (helm-buffers-list . "helm-buffers"))
	      :after
	      (progn
		(setq helm-autoresize-max-height 50
		      helm-autoresize-min-height 20
		      helm-display-buffer-default-height 20
		      helm-display-buffer-height 20
		      helm-M-x-fuzzy-match t
		      helm-buffers-fuzzy-matching t
		      helm-recentf-fuzzy-match t
		      helm-semantic-fuzzy-match t
		      helm-imenu-fuzzy-match t
		      helm-split-window-in-side-p nil
		      helm-move-to-line-cycle-in-source nil
		      helm-ff-search-library-in-sexp t
		      helm-scroll-amount 20
		      helm-echo-input-in-header-line nil)
		(helm-mode 1)
		))

;;;; helm-org
(auto-require 'helm-org
 	      :paths "helm"
 	      :reqby 'org
 	      :functions '((helm-org-completing-read-tags . "helm-org")))

;;;; helm-dash
;; must install sqlite3
(auto-require 'helm-dash
	      :paths '("helm-dash" "helm")
	      :functions '(helm-dash helm-dash-at-point)
	      :after
	      (progn
		(setq helm-dash-docsets-path locale-docset-dir
		      ;; helm-dash-common-docsets '("CMake")
		      helm-dash-min-length 3
		      helm-dash-browser-func 'browse-url-generic) ;; or 'eww

		(setq browse-url-browser-function 'browse-url-generic
		      browse-url-generic-program locale-browser-path)
		(defun emacs-lisp-dash ()
		  (interactive)
		  (setq-local helm-dash-docsets '("Emacs Lisp")))
		(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-dash)
		(add-hook 'lisp-interaction-mode-hook 'emacs-lisp-dash)
		))


;;;; load packages
(defvar is-enable-posframe nil)
(auto-require 'base-toolkit  :before (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))
;; (auto-require 'init-hydra    :before (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))
;; (auto-require 'init-keys     :before (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))
(auto-require 'init-locale   :load t :before (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))
;;(auto-require 'init-orgmode  :before (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))


;;;; encodings
(auto-require 'mule
	      :load t
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
		
		;; set coding config, last is highest priority.
		;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Recognize-Coding.html#Recognize-Coding
		(prefer-coding-system 'cp950)
		(prefer-coding-system 'gb2312)
		(prefer-coding-system 'cp936)
		(prefer-coding-system 'gb18030)
		(prefer-coding-system 'utf-16)
		(prefer-coding-system 'utf-8-dos)
		(prefer-coding-system 'utf-8-unix)
		
		;; 文件默认保存为 utf-8
		(set-buffer-file-coding-system 'utf-8-unix)
		(set-default buffer-file-coding-system 'utf-8-unix)
		(set-default-coding-systems 'utf-8-unix)
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
		  ;;(modify-coding-system-alist 'file "\\.txt\\'" 'chinese-iso-8bit-dos)
		  ;;(modify-coding-system-alist 'file "\\.h\\'" 'chinese-iso-8bit-dos)
		  ;;(modify-coding-system-alist 'file "\\.cpp\\'" 'chinese-iso-8bit-dos)
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

;;;; unicad
;(add-to-list 'load-path (concat auto-require-packages-dir "/unicad"))
;(require 'unicad)
;(unicad-enable)

(auto-require 'bookmark
	      :load nil
	      :after
	      (progn
		;; 自动保存书签
		(add-hook 'kill-emacs-hook
			  '(lambda ()
			     (bookmark-save)))
		))

;;;; ido
(auto-require 'ido
	      :load t
	      :before
	      (progn (setq ido-save-directory-list-file "~/tmp/emacs_cache/ido.last"))
	      :after
	      (progn
		(ido-mode t)
		(setq ido-enable-flex-matching t) ;; enable fuzzy matching
		(setq ido-auto-merge-delay-time 10000) ;; disable auto search file
		))

(auto-require 'smex
	      :load t
	      :before (add-package-path "smex")
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

(auto-require 'imenu
	      :after
	      (progn
		(add-to-list 'imenu-generic-expression '("sections" "^;;;; \\(.+\\)$" 1) t)
		(setq imenu-auto-rescan t
		      imenu-auto-rescan-maxout 500000)))

(auto-require 'dired
	      :load nil
	      :before
	      (progn
		(add-package-path '("emacs-async" "w32-browser"))
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
		;;(add-hook 'dired-mode-hook 'eye-dired-mode-setup)
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
;;(auto-require 'init-org-note
;;			  :before
;;			  (progn
;;				(autoload 'org-new-note "init-org-note" "" t)
;;				(autoload 'org-new-post "init-org-note" "" t)
;;				(autoload 'org-note-search-tag "init-org-note" "" t)
;;				(autoload 'org-note-search-title "init-org-note" "" t)
;;				))

;;;; org2nikola
;; 需要安装插件nikola plugin -i upgrade_metadata
;; 光标要放到标题节点上，导出时才有文件名，否则只有一个posts/.wp和posts/.meta文件
(auto-require 'org2nikola
	      :paths "org2nikola"
	      :reqby 'org
	      :functions '(org2nikola-export-subtree org2nikola-rerender-published-posts org2nikola-get-slug)
	      :after
	      (progn
		(setq org2nikola-output-root-directory "~/org/blog")
		(setq org2nikola-use-verbose-metadata t)
		(setq org2nikola-org-blog-directory "/mnt/windows/note/wikinote/nikola-blog")
		
		(defun org2nikola-after-hook-setup (title slug)
		  "see https://help.github.com/articles/setting-up-a-custom-domain-with-github-pages/ for setup
Run `ln -s ~/org/owensys.github.io ~/org/blog/output`"
		  (let ((url (concat "https://owensys.github.io/posts/" slug ".html"))
			(nikola-dir (file-truename "~/org/blog"))
			cmd)
		    ;; copy the blog url into kill-ring
		    (kill-new url)
		    (message "%s => kill-ring" url)
		    ;; nikola is building posts ...
		    (shell-command (format "source ~/nikola/bin/activate; cd %s; nikola build" nikola-dir))
		    (setq cmd "cd ~/org/owensys.github.io;git add .;git commit -m 'updated';git push origin master")
		    (shell-command cmd)
		    ))
		;;(add-hook 'org2nikola-after-hook 'org2nikola-after-hook-setup)

		(defun org2nikola-rerender-all-posts ()
		  "Re-render all old published posts."
		  (interactive)
		  (unless (and org2nikola-org-blog-directory
			       (file-directory-p org2nikola-org-blog-directory))
		    (setq org2nikola-org-blog-directory
			  (read-directory-name "Org blog directory:")))

		  (dolist (f (directory-files org2nikola-org-blog-directory t))
		    (when (and (not (file-directory-p f))
			       (string-match-p "\.org$" f))
		      ;; need setup default-directory to get absolute path of image embedded in org file
		      (let* ((default-directory (file-name-directory f)))
			(with-temp-buffer
			  ;; need make sure is org-mode is load loaded
			  (insert-file-contents f)
			  (org-mode)
			  (goto-char (point-min))
			  ;; 修复org2nikola-rerender-published-posts失败
			  ;; blog文件专门放了一个目录，不需要判断是否有POST_SLUG了，判断的话会失败
			  (if (search-forward-regexp "^\* [^ ]+" (point-max) t) ;; while=>if防止无限循环
			      (org2nikola-export-subtree)))))))

		))

;;;; keyfreq
(auto-require 'keyfreq
	      :paths "keyfreq"
	      :load t
	      :before
	      (add-package-path "keyfreq")
	      :after
	      (progn
		(keyfreq-mode 1)
		(keyfreq-autosave-mode 1)))

;;;; tramp
(auto-require 'tramp
	      :after
	      (progn
		(setq password-cache-expiry 360000000      ;设置密码过期时间，避免每次询问密码
		      tramp-default-method "ssh")
		))


;;;; site packages
(auto-require 'eno
	      :paths '("eno" "dash" "edit-at-point")
	      :functions '(eno-word-copy eno-word-copy-in-line eno-line-copy)
	      :before
	      (add-package-path '("edit-at-point" "dash" "eno"))
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
(auto-require 'all-the-icons
	      :paths "all-the-icons"
	      :before
	      (progn
		(if is-gui
		    (unless (or is-windows (member "all-the-icons" (font-family-list)))
		      (all-the-icons-install-fonts t))))
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
(auto-require 'solaire-mode
	      :paths "emacs-solaire-mode"
	      :functions 'solaire-mode
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
(when is-gui (auto-require 'doom-themes
			   :paths "emacs-doom-themes"
			   :load t
			   :functions '((doom-themes-visual-bell-config . "doom-themes-ext-visual-bell")
					(doom-themes-org-config . "doom-themes-ext-org"))
			   :after
			   (progn
			     ;; Enable flashing mode-line on errors
			     (doom-themes-visual-bell-config)
			     ;; Corrects (and improves) org-mode's native fontification.
			     (doom-themes-org-config)
				 ;;(load-theme 'doom-Iosvkem t)
				 (load-theme 'doom-nord-light t)
			     )))

 ;;;; doom-modeline
(when is-gui (auto-require 'doom-modeline
 						   :paths '("doom-modeline" "all-the-icons" "eldoc-eval" "shrink-path" "emacs-memoize" "s" "f" "dash")
 						   :load t
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
							 )))


;; (when is-terminal
;; (set-face-attribute 'hl-line nil :background "darkgray"))

;;;; avy
(auto-require 'avy
	      :paths "avy"
	      :functions '(avy-goto-char avy-goto-line))

;;;; avy-zap
(auto-require 'avy-zap
	      :paths "avy-zap"
	      :functions '(avy-zap-to-char))

;;;; ace-jump
(auto-require 'ace-jump
	      :paths "ace-jump-mode"
	      :functions 'ace-jump-mode)

;;;; ido-menu
(auto-require 'init-idomenu)

;;;; ivy swiper counsel
(auto-require 'swiper
	      :paths "swiper"
	      :functions '(counsel-imenu counsel-ag counsel-rg counsel-git)
	      :after
	      (progn
		(require 'smex)
		(require 'counsel)
		(require 'swiper)
		(setq ivy-initial-inputs-alist nil) ;;不需要自动添加^符号
		(setq ivy-count-format "(%d/%d)") ;; display both the index and the count
		))

;;;; super-save
;;(auto-require 'super-save
;;	      :load t
;;	      :paths "super-save"
;;	      :after
;;	      (progn
;;		(setq super-save-remote-files nil)
;;		(super-save-mode 1)))

(auto-require 'symbol-overlay
	      :paths "symbol-overlay"
	      :functions '(symbol-overlay-mode symbol-overlay-put))

;;;; bm
(auto-require 'bm
	      :paths "bm"
	      :functions '(bm-toggle bm-next bm-previous)
	      :before
	      (progn
		(autoload 'counsel-bm "ext-bm")
		(setq bm-cycle-all-buffers nil		;; 是否在所有buffer中循环
		      ;; (setq bm-in-lifo-order t)		;; 先入先出
		      bm-restore-repository-on-load t
		      ;; where to store persistant files
		      bm-repository-file "~/.emacs.d/bm-repository"
		      ;; save bookmarks
		      bm-buffer-persistence t)
		)
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
		))	   

;;;; ibuffer
(auto-require 'ibuffer
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

;;;; hungry-delete
(auto-require 'hungry-delete
	      :load t
	      :paths "hungry-delete"
	      :after (global-hungry-delete-mode 1))

;;;; rainbow-mode
(when is-gui (auto-require 'rainbow-mode
			   :paths "rainbow-mode"
			   :functions 'rainbow-mode))

;;;; aweshell
(auto-require 'aweshell
	      :paths "aweshell"
	      :functions '(aweshell-new aweshell-next aweshell-prev aweshell-toggle))

;;;; web search
(auto-require 'init-web-search)

;;;; watch-other-window
(auto-require 'watch-other-window
	      :paths "watch-other-window"
	      :functions '(watch-other-window-up watch-other-window-down watch-other-window-up-line watch-other-window-down-line))

;;;; rg
(auto-require 'rg
	      :paths "rg"
	      :functions 'rg)

;;;; color-rg
(auto-require 'color-rg
	      :paths "color-rg"
	      :functions '(color-rg-search-input))

(auto-require 'init-elisp :load t)

;;;; orgmode
(auto-require 'init-orgmode :load t)
(auto-require 'init-my-orgwiki :load t)

;; Advise set auto-save-default to nil
(auto-require 'org-crypt
	      :reqby 'org
	      :after
	      (progn
		(org-crypt-use-before-save-magic)
		(setq org-tags-exclude-from-inheritance (quote("crypt")))
		(setq org-crypt-key nil)
		(setq org-crypt-tag-matcher "sec") ;; Custom tag for crypt
		))

(auto-require 'helm-org
	      :paths "helm")

;;;; deft
(auto-require 'deft
	      :paths "deft"
	      :functions 'deft
	      :after
	      (progn
		(setq deft-recursive t)
		(setq deft-use-filename-as-title t) ;;是否把文件名作为标题
		(setq deft-extensions '("txt" "tex" "org"))
		(setq deft-directory locale-notebook-dir)
		(setq deft-file-limit 200) ;;最多显示多少文件，nil不限制
		(setq deft-filter-only-filenames t) ;;只搜索文件名
		(setq deft-auto-save-interval 0) ;;是否自动保存从deft打开的文件
		(setq deft-current-sort-method 'mtime) ;;排序方式
		(setq deft-strip-summary-regexp ".*"))
	      )

(defun eye/deft-search(filter)
  (interactive "MFilter: ")
  (deft)
  (deft-filter filter t))

(defun deft-or-close ()
  (interactive)
  (if (eq major-mode 'deft-mode)
      (progn (kill-buffer "*Deft*"))
    (deft)
    ))

;;;; notdeft
;; 安装方法
;; git clone https://github.com/hasu/notdeft.git
;; sudo apt install libtclap-dev libxapian-dev
;; pushd notdeft/xapian
;; make
;; popd
(auto-require 'notdeft
			  :load t
			  :paths "notdeft"
			  :functions 'notdeft
			  :before
			  (require 'notdeft-autoloads)
			  :after
			  (progn
				(require 'notdeft)
				(setenv "XAPIAN_CJK_NGRAM" "1") ;; 通过环境变量支持中文搜索，也可以修改源代码https://emacs-china.org/t/notdeft/11314
				(setq notdeft-xapian-program (expand-file-name "notdeft/xapian/notdeft-xapian" auto-require-packages-dir)) ;; 设置notdef-xapian程序名
				(setq-default notdeft-directories `(,(expand-file-name locale-notebook-dir)))
				(require 'notdeft-org)
				(require 'notdeft-global-hydra)
				;; 通过notdeft自动创建文件时，使用输入的字符串作文件名
				(setq notdeft-notename-function '(lambda (str) str))
				(defun notdeft-file-summary (file) "") ;; 不显示摘要信息
				(setq notdeft-xapian-max-results 0) ;; No limit if 0
				))



;; password-generator
(auto-require 'password-generator
	      :paths "emacs-password-generator"
	      :functions '((password-generator-simple . "password-generator")
			   (password-generator-strong . "password-generator")
			   (password-generator-paranoid . "password-generator")
			   (password-generator-numeric . "password-generator")))

(defun eye/open-password-file ()
  "Open my password manager file"
  (interactive)
  (find-file (expand-file-name "password.org" locale-notebook-dir)))


;;;; yasnippet
(auto-require 'yasnippet
	      :paths "yasnippet"
	      :functions '(yas-minor-mode yas-global-mode))

;;;; ivy-yasnippet
(auto-require 'ivy-yasnippet
	      :paths '("ivy-yasnippet" "swiper" "dash" "yasnippet")
	      :reqby 'yasnippet
	      :functions 'ivy-yasnippet)

;;;; yankpad
(auto-require 'yankpad
	      :paths "yankpad"
	      :reqby 'org
	      :functions 'yankpad-insert
	      :after
	      (progn
		;; ~/.emacs.d/snippets is yas--default-user-snippets-dir
		;;如果手动更换orgmode9后，这句执行后出现Not a face: nil的奇怪问题，终端下ivy无法弹出来，如果是赋值为不带/的字符串，又不会出现问题
		(setq yankpad-file (expand-file-name "Yankpad.org" locale-notebook-dir))
		;; (add-to-list 'hippie-expand-try-functions-list #'yankpad-expand)
		))


;;;; lisp-mode
(require 'lisp-mode
	 :after
	 (progn
	   (defun setup-lisp-mode ()
	     (setq tab-with 2))
	   (add-hook 'emacs-lisp-mode-hook 'setup-lisp-mode)))


;;;; cc-mode cpp
(auto-require 'cc-mode
	      :before
	      (require 'init-cpp))

;;;; rainbow-delimiters
;; 括号高亮
(auto-require 'rainbow-delimiters
	      :load t
	      :paths "rainbow-delimiters"
	      :functions 'rainbow-delimiters-mode
	      :before
	      (progn
		(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

;;;; highlight-numbers
(when is-gui (auto-require 'highlight-numbers
			   :paths '("highlight-numbers" "parent-mode")
			   :functions 'highlight-numbers-mode
			   :before
			   (progn
			     (add-hook 'prog-mode-hook 'highlight-numbers-mode))))

;;;; whitespace
;; http://ergoemacs.org/emacs/whitespace-mode.html
(auto-require 'whitespace
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
(auto-require 'company-c-headers
	      :paths '("company-c-headers" "company")
	      :reqby 'company
	      :after
	      (progn
		(add-hook 'c++-mode-hook (lambda () (add-to-list 'company-backends 'company-c-headers)))
		(when is-windows
		  (setq company-c-headers-path-system '("C:/Program Files (x86)/Microsoft Visual Studio 14.0/VC/include"
							"C:/Program Files (x86)/Microsoft SDKs/Windows\v7.1A/Include")))))


;;;; php-mode
(auto-require 'php-mode
	      :paths "php-mode"
	      :functions 'php-mode
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
(auto-require 'qt-pro-mode
	      :paths "qt-pro-mode"
	      :reqby 'cc-mode
	      :functions 'qt-pro-mode
	      :before
	      (progn
		(add-to-list 'auto-mode-alist '("\\.pro$" . qt-pro-mode))
		(add-to-list 'auto-mode-alist '("\\.pri$" . qt-pro-mode))     
		(add-hook 'qt-pro-mode-hook 'setup-qt-pro-mode)))

;;;; css-mode
(auto-require 'css-mode
	      :before
	      (add-to-list 'auto-mode-alist '("\\.qss$" . css-mode)))

;;;; qml-mode
(auto-require 'qml-mode
	      :paths "qml-mode"
	      :functions 'qml-mode
	      :before
	      (progn
		(add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))))

;;;; etags
(defun eye/create-ctags-file ()
  "Create ctags file"
  (interactive)
  ;; ctags必须加-e选项，否则counsel-xxx-find-tag-xx无法识别其中的tagname
  (let ((tags-dir (ido-read-directory-name "TAGS DIR:"))
	;; 需要传"\\("，否则出现错误：bash: -c:行0: 未预期的符号 `(' 附近有语法错误
	(command "find %s \\( -iwholename \"*.h\" -or -iwholename \"*.cpp\" \\) -print | ctags -e -f %sTAGS -V -R -L -"))
    (setq command (format command tags-dir tags-dir))
    (message command)
    (let ((proc (start-process "ctags" nil shell-file-name shell-command-switch command)))  ;; shell-command-switch值为-c，表示后面的是命令行参数
      (set-process-sentinel proc `(lambda (proc msg)
				    (let ((status (process-status proc)))
				      (when (memq status '(exit signal))
					(message "ctags:%s" msg)
					)))))
      ;;    (async-shell-command command)
    ))


(defun eye/update-ctags-this-file ()
  "Update current file tags"
  (interactive)
  (let ((tags-path (locate-dominating-file default-directory "TAGS"))
	(command)
	(proc))
    (when tags-path
      (setq tags-path (expand-file-name "TAGS" tags-path))
      (setq command (format "ctags -e -a -f %s %s" tags-path (buffer-name))) ;; -a means append
      (message (concat "custom command:" command))
      (async-shell-command command)
      (delete-other-windows))))

;; company-etags要么使用当前项目的TAGS，要么使用tags-table-list定义的TAGS文件，所以干脆直接配置tags-table-list
;;(if locale-system-tags-paths
;;    (append-to-list 'tags-table-list locale-system-tags-paths)) ;; need load init-locale
;; Don't ask before rereading the TAGS files if they have changed
(setq tags-revert-without-query t)
;; Do case-sensitive tag searches
(setq tags-case-fold-search nil) ;; t=case-insensitive, nil=case-sensitive
;; Don't warn when TAGS files are large
(setq large-file-warning-threshold nil)

(defun eye/load-project-root-tags ()
  "加载本地文件上层目录中对应的TAGS文件"
  (interactive)
  (let ((root-tags-file (locate-dominating-file default-directory "TAGS")))
    (when root-tags-file
      (setq root-tags-file (concat root-tags-file "TAGS"))
      (message "Loading tags file: %s" root-tags-file)
      (visit-tags-table root-tags-file)
      (add-to-list 'tags-table-list root-tags-file)
      (add-to-list 'tags-table-files root-tags-file) ;; for find-file-in-tags
      )))

;;;; counsel-etags
(auto-require 'counsel-etags
	      :paths "counsel-etags"
	      :functions '(counsel-etags-find-tag counsel-etags-find-tag-at-point)
	      :after
	      (progn
		;; Don't ask before rereading the TAGS files if they have changed
		(setq tags-revert-without-query t)
		;; Don't warn when TAGS files are large
		(setq large-file-warning-threshold nil)
		(when is-linux
		  (setq counsel-etags-tags-program "xargs etags --append") ;调用命令类似 find .... -print | xargs etags --append, etags没有递归的参数
		  )
		(when is-windows (setq counsel-etags-tags-program (executable-find "ctags"))) ;; if not set, (counsel-etags-guess-program "ctags") find failed

		;; 是否开启输出命令
		(setq counsel-etags-debug nil)

		;;(append-to-list 'counsel-etags-extra-tags-files locale-system-tags-paths) ;;使counsel-etags能显示系统函数（但无法跳转进入）
		
		;; Setup auto update now
		(setq counsel-etags-update-tags-backend 'eye/update-ctags-this-file)  
		(add-hook 'c++-mode-hook
			  (lambda ()
			    (add-hook 'after-save-hook 'counsel-etags-virtual-update-tags 'append 'local)
			    ;;(add-hook 'after-save-hook 'eye/update-ctags-this-file))
			    ))
		
		;; counsel-etags-ignore-directories does NOT support wildcast
		(dolist (dirname (list ".git" ".svn" ".vs" "ipch" "Debug" "Release" "Bin" "tmp"))
		  (add-to-list 'counsel-etags-ignore-directories dirname))

		;; counsel-etags-ignore-filenames supports wildcast
		(dolist (filename (list "TAGS" "GPATH" "GTAGS" "*.json" "ui_*.h" "*.ui" "moc_*.cpp" "*.rc"
					"*.qrc" "*.tlog" "*.md" "*.bat" "*.txt" "*.pdb" "*.filters" "*.user"
					"*.vcproj" "*.vcxproj" "*.db" "*.opendb" "*.htm" "*.user" "*.make"
					"*.sln" "*.exp" "*.sdf" "*.opensdf"))
		  (add-to-list 'counsel-etags-ignore-filenames filename))
		))


;;;; company
(auto-require 'init-company
	      :paths "company-mode"
	      :load t
	      :functions '((global-company-mode . "company")))

;;;; company-quickhelp
(auto-require 'company-quickhelp
	      :before
	      ;;company-quickhelp-manual-begin is not autoload function, must define.
	      (autoload 'company-quickhelp-manual-begin "company-quickhelp" "quickhelp" t)
	      :after
	      (progn
		;; 手动触发显示
		(setq company-quickhelp-delay nil)
		(define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin)))

;;;; ffit
(auto-require 'find-file-in-tags
	      :paths "find-file-in-tags"
	      :functions 'find-file-in-tags)

;;;; ffip
(auto-require 'find-file-in-project
	      :paths "find-file-in-project"
	      :functions 'find-file-in-project
	      :before
	      (progn
		;; Windows平台必须设置，否则执行ffip会直占用CPU。
		(when is-windows (setq ffip-find-executable "find"))
		(setq ffip-find-executable "find")))

;;;; awesome-tab
(when is-gui (auto-require 'awesome-tab
			   :load t
			   :paths "awesome-tab"
			   :before
			   (progn
			     (setq awesome-tab-style 'zigzag))
			   :after
			   (progn
			     (awesome-tab-mode 1)
			     (define-key global-map (kbd "<C-tab>") #'awesome-tab-forward-tab)
			     (define-key global-map (kbd "<C-S-iso-lefttab>") #'awesome-tab-backward-tab)
			     )))

;;;; awesome-tray
(when is-gui (auto-require 'awesome-tray
			   :load t
			   :paths "awesome-tray"
			   :functions 'awesome-tray-mode
			   :after
			   (progn
			     ;; Create a module to show file encoding
			     (defun tray-module-encode-info ()
			       (format "%s" buffer-file-coding-system))
			     (defface tray-module-encode-face
			       '((((background light))
				  :foreground "#00a400" :bold t)
				 (t
				  :foreground "green3" :bold t))
			       "Encode name face."
			       :group 'awesome-tray)			     
			     (add-to-list 'awesome-tray-module-alist
					  '("encode" . (tray-module-encode-info tray-module-encode-face)))
			     (add-to-list 'awesome-tray-active-modules "encode")

			     ;; Create a module to show current input method
			     (defun tray-module-im-info ()
			       (if current-input-method
				   (format "%s" current-input-method)
				 "En"))
			     (add-to-list 'awesome-tray-module-alist
					  '("im" . (tray-module-im-info)))
			     (add-to-list 'awesome-tray-active-modules "im")

			     ;; set color
			     (setq awesome-tray-mode-line-active-color "DarkGreen")
				   
			     ;; Enable awesome-tray-mode
			     ;;(awesome-tray-mode 1)
			     )))

;;;; nox
(when (and is-linux is-gui (executable-find "nox"))
  (auto-require 'nox
		:paths '("posframe" "company-mode" "nox")
		:functions '((nox-ensure . "nox"))
		:before
		(progn
		  (dolist (hook (list
				 'c-mode-common-hook
				 'c-mode-hook
					    'c++-mode-hook
					    ))
		    (add-hook hook '(lambda () (nox-ensure))))
		  )))


;;;; global-readonly
(auto-require 'global-readonly
	      :paths "global-readonly"
	      :functions 'global-readonly-toggle)

;;;; treemacs
(auto-require 'treemacs
	      :paths '("s" "f" "ht" "ace-window" "pfuture" "treemacs/src/elisp")
	      :functions '((treemacs . "treemacs"))
	      :after
	      (progn
		(bind-key treemacs-mode-map "<right>" 'treemacs-RET-action)))

;;;; writeroom
(auto-require 'writeroom-mode
	      :paths '("writeroom-mode" "visual-fill-column")
	      :functions 'writeroom-mode
	      :before
	      (progn
			(setq writeroom-width 120)
			(defun writeroom-mode-on ()
			  (interactive)
			  (add-hook 'prog-mode-hook 'writeroom-mode)
			  (writeroom-mode))
			(defun writeroom-mode-off ()
			  (interactive)
			  (remove-hook 'prog-mode-hook 'writeroom-mode)
			  (writeroom-mode -1))
			))

;;;; page-break-lines
(auto-require 'page-break-lines
	      :paths "page-break-lines"
	      :after
	      (setq page-break-lines-char ?=)) ;避免只显示一半

;;;; dashboard
(auto-require 'dashboard
	      :load t
	      :paths '("emacs-memoize" "emacs-dashboard")
	      :after
	      (progn
		(setq dashboard-startup-banner (concat user-emacs-directory "res/moleskine_red_notebook.png")
		      dashboard-banner-logo-title ""
		      dashboard-footer "Life is what you make it!"
		      dashboard-center-content    t
		      dashboard-set-heading-icons t
		      dashboard-set-file-icons    t
		      dashboard-set-init-info     t
		      dashboard-items '((recents   . 10)
					(bookmarks . 5)))
		(dashboard-setup-startup-hook)
		))

;;;; bing-dict
(auto-require 'bing-dict
	      :paths "bing-dict"
	      :functions 'bing-dict-brief)

;;;; snails
;; 使用fuz.el，emacs需要支持动态模块，编译时以--with-modules编译，用(fboundp 'module-load)
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Dynamic-Modules.html
(when is-gui (auto-require 'snails
			   :paths '("fuz" "snails")
			   :functions '((snails . "snails"))
			   :after
			   (progn
			     (require 'snails-backend-rg)

			     (define-key snails-mode-map (kbd "<down>") #'snails-select-next-item)
			     (define-key snails-mode-map (kbd "<up>") #'snails-select-prev-item)

			     (define-key snails-mode-map (kbd "<M-down>") #'snails-select-next-backend)
			     (define-key snails-mode-map (kbd "<M-up>") #'snails-select-prev-backend)
			     )))

;;;; pyim
(auto-require 'pyim
	      :paths '("posframe" "xr" "pyim" "pyim-wbdict")
	      :load t
	      :before
	      (progn
		(setq default-input-method "pyim")
		;; 使用 popup-el 来绘制选词框, 如果用 emacs26, 建议设置
		;; 为 'posframe, 速度很快并且菜单不会变形，不过需要用户
		;; 手动安装 posframe 包。
		(setq pyim-page-tooltip 'posframe)
		;; 选词框显示5个候选词
		(setq pyim-page-length 5)
		
		;; 设置五笔输入模式，另外，在使用五笔输入法之前，还需要用 pyim-dicts-manager 添加一个五笔词库，选择pyim-wbdict中的pyim文件
		;; 保存后会得到一个配置文件，里面有设置了pyim-dicts这个变量，所以也可以直接设置这个变量
		(setq pyim-default-scheme 'wubi)
		(setq pyim-dicts (quote
				  ((:name "qingge" :file "~/.emacs.d/packages/pyim-wbdict/pyim-wbdict-qingge.pyim")
				   (:name "freeime" :file "~/.emacs.d/packages/pyim-wbdict/pyim-wbdict-freeime.pyim"))))
		;;(pyim-restart)
		))

;;;; tempbuf
(auto-require 'tempbuf
	      :paths "tempbuf"
	      :before
	      (progn
		(setq tempbuf-kill-message nil)         ;不在Mode-line显示删除临时buffer提示消息
		(setq tempbuf-minimum-timeout 30)       ;删除 buffer 的最低期限
		(dolist (hook (list
			       'compilation-mode-hook     ;编译模式
			       'comint-mode-hook          ;comint 模式
			       'completion-list-mode-hook ;补全列表模式
			       'help-mode-hook            ;帮助模式
			       'Info-mode-hook            ;Info 模式
			       'woman-mode-hook		  ;man 手册
			       ))
		  (add-hook
		   hook
		   '(lambda ()
		      (require 'tempbuf)
		      (turn-on-tempbuf-mode))))         ;加载自动清理临时buffer
		))


(auto-require 'eaf
	      :paths "emacs-application-framework"
	      :functions '((eaf-open-browser . "eaf")
			   (eaf-open-terminal . "eaf"))
	      )


;;;; external
(auto-require 'init-external)

;;;; magit 
(when is-linux (auto-require 'magit
			     :paths '("magit/lisp" "with-editor" "transient/lisp")
			     :functions '((magit-status . "magit-status"))
			     :after
			     (progn
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
			       ;;(setq magit-display-buffer-function #'magit-display-buffer-pop-up-frame)
			       ;;(define-key magit-mode-map (kbd "q") 'delete-frame) ;; 自动关闭 frame
			       
			       (setq magit-display-buffer-function #'magit-display-buffer-traditional)

			       )))

;;;; apt-utils
(when is-linux  (auto-require 'apt-utils
			      :paths "apt-utils"
			      :functions '(apt-utils-search apt-utils-show-package)))


;;;; editorconfig
(auto-require 'editorconfig
	      :paths "editorconfig"
	      :load t
	      :after
	      (editorconfig-mode 1))


;;;; slime
(auto-require 'slime
	      :paths "slime"
	      :functions 'slime
	      :after
	      (progn
		(setq inferior-lisp-program "/usr/bin/sbcl")))

;;;; newlisp
(auto-require 'newlisp-mode
	      :paths "newlisp-mode"
	      :functions '(run-newlisp)
	      :after
	      (progn
		(add-to-list 'auto-mode-alist '("\\.lsp\\'" . newlisp-mode))
		(define-key newlisp-mode-map (kbd "C-x C-e") #'newlisp-eval-last-sexp)
		(define-key newlisp-mode-map (kbd "<f5>") #'newlisp-eval-buffer)
		))

(require 'init-utils)

;;;; idle load packages
;; for quick startup
(setq idle-load-features '(server
			   saveplace
			   recentf
			   savehist
			   org org-capture org-agenda))

(idle-load-startup)


(provide 'configuration)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; configuration.el ends here
