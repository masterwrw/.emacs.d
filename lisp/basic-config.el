;;; basic-config.el --- My emacs minimal configuration -*- lexical-binding: t -*-

;; Produce backtraces when errors occur
(setq debug-on-error t)

;;;; startup
;; Speed up startup
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


;;
;; 启动时间统计
;;
;; 自定义计算时间
(defvar init-start (current-time))
(add-hook 'after-init-hook
          (lambda ()
            (message (format "\n\nEmacs startup time: %.6f seconds.\n\n\n" (- (float-time (current-time)) (float-time init-start))))
            ))

;;;; system env
(setq is-windows (or
		  (eq system-type 'windows-nt)
		  (eq system-type 'cygwin)))
(setq is-linux (eq system-type 'gnu/linux))
(setq is-mac (eq system-type 'darwin))

(setq is-gui (display-graphic-p))
(setq is-terminal (not (display-graphic-p)))

;;;; load custom-file before all init-* config
(setq custom-file (concat user-emacs-directory "custom-set-variables-terminal.el"))
(unless (file-exists-p custom-file)
					;(f-touch custom-file)
  (shell-command (concat "touch " custom-file))
  )
(load custom-file t t)

;;;; packages
;;(require 'package)
;;(setq package-enable-at-startup nil) ; not activat installed packages
;;(setq package-archives
;;      '(("gnu" . "http://elpa.emacs-china.org/gnu/")
;;	("melpa" . "http://elpa.emacs-china.org/melpa/")
;;	 ("melpa-stable" . "http://elpa.emacs-china.org/melpa-stable/")))
;;(package-initialize) ; activate installed packages
;;;; On-demand installation of packages
;;(defun require-package (package &optional min-version no-refresh)
;;  "Ask elpa to install given PACKAGE."
;;  (if (package-installed-p package min-version)
;;      t
;;    (if (or (assoc package package-archive-contents) no-refresh)
;;        (package-install package)
;;      (progn
;;        (package-refresh-contents)
;;        (require-package package min-version t)))))


;;;; ido
;;(require 'ido)
;;(ido-mode t)
;;(setq ido-enable-flex-matching t) ;; enable fuzzy matching
;;(setq ido-auto-merge-delay-time 10000) ;; disable auto search file
;;(setq ido-enable-last-directory-history nil) ;; don't save direcotry history
;;(setq ido-enable-dot-prefix t) ;; alow match dot files



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

;;(put 'suspend-frame 'disabled t) ;; 禁止 Ctrl+z 时挂起 emacs

;; 用 y/n 代替 yes/no 的回答
(defalias 'yes-or-no-p 'y-or-n-p) ;; (fset 'yes-or-no-p 'y-or-n-p) 相同的效果

(setq ring-bell-function 'ignore) ;; 禁止出现烦人的响铃

(setq truncate-lines t) ;; 不自动折行
(setq-default truncate-lines t)
(toggle-truncate-lines 1)

;; Fix load slow, https://github.com/raxod502/radian/issues/180
(when tool-bar-mode (tool-bar-mode -1)) ;; 禁用工具栏
(when menu-bar-mode (menu-bar-mode -1)) ;; 禁用菜单栏
(when scroll-bar-mode (scroll-bar-mode -1)) ;; 禁用滚动条
(when is-gui (setq frame-title-format "Editor")) ;; 自定义标题栏

;; 去掉窗口边缘和分割窗口时分割条的边缘
;; http://emacsredux.com/blog/2015/01/18/customizing-the-fringes/
;; linux maybe need gdb, and use mouse to set breakpoint on fringe, so only hide fringe on windows.
(when is-gui
  (set-window-fringes nil 0 0) ;; border side
  (fringe-mode '(0 . 0)) ;; middle of split frame
  )

(when is-gui
  (blink-cursor-mode -1) ;; 取消光标闪烁
  (add-hook 'after-init-hook
	    (lambda ()
	      (set-cursor-color "#00A876"))))

(setq mouse-yank-at-point t) ;; 强制粘贴时粘贴到光标处

;; @see https://stackoverflow.com/questions/2081577/setting-emacs-split-to-horizontal
;;(setq split-width-threshold nil) ;;不允许自动左右分屏
;;(setq split-height-threshold nil) ;;不允许自动上下分屏

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


;; display the real names on mode-line when visiting a symbolink
(setq find-file-visit-truename t)


;; 最大化
(when is-gui
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

  (when is-gui (add-hook 'after-init-hook 'maximize-frame))

  )


;;;; font
(when is-gui
  (when is-linux
    (setq en-font-name "Inconsolata")
    (setq cn-font-name "YaHei Consolas Hybrid")
    (setq en-font-size 14)
    (setq cn-font-size 12)
    )
  (when is-windows
    ;; Inconsolata
    ;; Fira Code
    ;; Droid Sans Mono Wide
    (setq en-font-name "Inconsolata")
    (setq cn-font-name "Microsoft YaHei")
    (setq en-font-size 16)
    (setq cn-font-size 14)
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
    (if (display-graphic-p)
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
    (setq en-font-size (- en-font-sizeeval 1))
    (setq cn-font-size (- cn-font-size 1))
    (eye-update-font-size)
    (if (equal (frame-parameter nil 'fullscreen) 'maximize)
	(maximize-frame))
    )
  
  (eye-update-font-size)
  )


(add-to-list 'load-path "~/src/emacs-packages/naysayer-theme")
(require 'naysayer-theme)
(load-theme 'naysayer t)
;; naysayer no header-line face, define it, colors in naysayer-theme.el
(set-face-attribute 'header-line nil :background "#d2b58d" :foreground "#082628")


;;(set-face-attribute 'default nil :background "grey0" :foreground "#ffffff")

;;;; History
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

(require 'saveplace)
(setq save-place-file (expand-file-name "places-terminal" user-emacs-directory))
(save-place-mode 1)

;;(require 'recentf)
(autoload 'recentf-open-files "recentf.el" nil t)
(with-eval-after-load 'recentf
  (setq recentf-max-saved-items 200)
  (setq recentf-save-file (expand-file-name "recentf-terminal" user-emacs-directory))
  ;;(add-to-list 'recentf-exclude (expand-file-name package-user-dir))
  (add-to-list 'recentf-exclude ".cache")
  (add-to-list 'recentf-exclude ".cask")
  (add-to-list 'recentf-exclude "bookmarks")
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'"))


;; save minibuffer history
;;(require 'savehist)
;;(setq savehist-file (expand-file-name "history-terminal" user-emacs-directory))
;;(setq enable-recursive-minibuffers t ; Allow commands in minibuffers
;;      history-length 100
;;      savehist-additional-variables '(mark-ring
;;				      global-mark-ring
;;				      search-ring
;;				      regexp-search-ring
;;				      extended-command-history)
;;      savehist-autosave-interval nil ;;不开启自动保存，否则会不断的分配内存
;;      )


;; for quick startup
;;(recentf-mode 1)
;;(savehist-mode 1)

;;;; Backup
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

(global-auto-revert-mode 1)

(setq delete-by-moving-to-trash t)	;删除文件或目录时，移到回收站


;;;; Encoding
(setq locale-coding-system 'utf-8)     ;; 设置emacs 使用 utf-8
(set-language-environment 'Chinese-GB) ;; 设置为中文简体语言环境
(set-keyboard-coding-system 'utf-8)    ;; 设置键盘输入时的字符编码
;; 解决粘贴中文出现乱码的问题
(if is-windows
    (progn
      ;; (setq selection-coding-system 'utf-16le-dos) ;; 修复从网页剪切文本过来时显示 \nnn \nnn 的问题
      ;; (set-default selection-coding-system 'utf-16le-dos)
      (set-selection-coding-system 'utf-16le-dos) ;; 别名set-clipboard-coding-system
      )
  (set-selection-coding-system 'utf-8))
(prefer-coding-system 'utf-8)
(prefer-coding-system 'gbk)
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
  (modify-coding-system-alist 'file "\\.txt\\'" 'gbk)
  (modify-coding-system-alist 'file "\\.h\\'" 'gbk)
  (modify-coding-system-alist 'file "\\.cpp\\'" 'gbk)
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

;;;; modeline
;; Copy from https://github.com/redguardtoo/emacs.d/blob/master/lisp/init-modeline.el
;; @see http://emacs-fu.blogspot.com/2011/08/customizing-mode-line.html
;; But I need global-mode-string,
;; @see http://www.delorie.com/gnu/docs/elisp-manual-21/elisp_360.html
;; use setq-default to set it for /all/ modes


;;(setq header-line-format mode-line-format)
;;(force-mode-line-update)
;; hide modeline
(setq mode-line-format nil)
(setq-default mode-line-format nil)

(setq-default header-line-format
              (list
	       ;;"%e"
	       ;;mode-line-front-space
	       " ["
               ;; the buffer name; the file name as a tool tip
               '(:eval (propertize (if (buffer-modified-p)
                                       "%b *"
                                     "%b")
                                   'face nil
                                   'help-echo (buffer-file-name)))
	       "] "

               '(:eval (format "%s" buffer-file-coding-system))
               
               " "
               
               ;; the current major mode for the buffer.
               "["

               '(:eval (propertize "%m" 'face nil
                                   'help-echo buffer-file-coding-system))
               " "


               ;; insert vs overwrite mode, input-method in a tooltip
               '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                                   'face nil
                                   'help-echo (concat "Buffer is in "
                                                      (if overwrite-mode "overwrite" "insert") " mode")))

               ;; is this buffer read-only?
               '(:eval (when buffer-read-only
                         (concat ","  (propertize "RO"
                                                  'face nil
                                                  'help-echo "Buffer is read-only"))))
               "] "
	       "%n " ;; narrow state
	       
	       ;; line and column
               "" ;; '%02' to set to 2 chars at least; prevents flickering
               "L%02l";; "," "%01c"
               ;; (propertize "%02l" 'face 'font-lock-type-face) ","
               ;; (propertize "%02c" 'face 'font-lock-type-face)
               " "

	       " "
	       ;;global-mode-string, org-timer-set-timer in org-mode need this
               ;;(propertize "%M" 'face nil)
	       ;;'(:eval (if (and (boundp 'simple-fly-insert-state-p) simple-fly-insert-state-p)
		;;	   "INSERT" "C"))
	       
               ;;" --"
               ;; i don't want to see minor-modes; but if you want, uncomment this:
               ;; minor-mode-alist  ;; list of minor modes
               ;;"%-" ;; fill with '-'
               ))

;;;; bookmark
(require 'bookmark)
(setq bookmark-file (expand-file-name "bookmarks-terminal" user-emacs-directory))
(setq bookmark-default-file (expand-file-name "bookmarks-terminal" user-emacs-directory))

;; 自动保存书签
(add-hook 'kill-emacs-hook
          '(lambda ()
             (bookmark-save)))



;;;; imenu
(require 'imenu)
(setq imenu-auto-rescan t)
(setq imenu-auto-rescan-maxout 500000)


;;;; dired
(require 'dired)
(require 'wdired)  ;; 支持在dired-mode中批量重命名
(require 'dired-x) ;; 支持 dired-jump 进入后自动定位到当前文件名位置

;;;; local
(setq user-full-name "owensys")
(setq user-mail-address "owensys@hotmail.com")

;;(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(when is-linux
  (require 'tramp)
  (setq password-cache-expiry 360000000)      ;设置密码过期时间，避免每次询问密码
  (setq tramp-default-method "ssh"))

;;;; cpp
(require 'cc-mode)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(defun eye/find-header-or-source-file (&optional is-open-other-window)
  "Find the header or source file of this one."
  (interactive "P")
  (let ((full-base-name (file-name-sans-extension buffer-file-name)) ;;无后缀的文件路径
	(header-or-source-path nil))
    
    (cond ((string-match "\\.h" buffer-file-name)
	   (if (file-exists-p (concat full-base-name ".c"))
	       (setq header-or-source-path (concat full-base-name ".c"))
	     (setq header-or-source-path (concat full-base-name ".cpp"))))
	  
	  ((string-match "\\.c" buffer-file-name)
	   (setq header-or-source-path (concat full-base-name ".h")))
	  
	  ((string-match "\\.cpp" buffer-file-name)
	   (setq header-or-source-path (concat full-base-name ".h")))

	  (t (message "File name no suffix")))
    (if header-or-source-path
	(if is-open-other-window (find-file-other-window header-or-source-path)
	  (find-file header-or-source-path))
      (error "Unable to find a header or source file"))))

(defun set-tab-width-hook ()
  (setq indent-tabs-mode nil)
  (setq default-tab-width 2)
  (setq tab-width 2)
  (setq c-basic-offset 2) ;; tab 缩进量
  (setq c-default-style "k&r") ;; 大括号缩进位置，https://en.wikipedia.org/wiki/Indentation_style
  (setq tab-stop-list nil))

(defun eye-setup-c++ ()
  (define-key c++-mode-map (kbd "<M-up>") 'beginning-of-defun)
  (define-key c++-mode-map (kbd "<M-down>") 'end-of-defun)
  ;;(define-key c++-mode-map (kbd "<f5>") 'make-without-asking)

  (add-hook 'c-mode-hook 'set-tab-width-hook)
  (add-hook 'c++-mode-hook 'set-tab-width-hook)
  )

(eval-after-load 'cc-mode (eye-setup-c++))


;;;; extend functions
(defun eye/kill-inner-word ()
  "Kills the entire word your cursor is in. Equivalent to 'ciw' in vim."
  (interactive)
  ;;(forward-char 1) 
  (backward-word)
  (kill-word 1))


(defun k-scratch ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))


(defun k-full-path ()
  "Show the file full path with current buffer."
  (interactive)
  (message (expand-file-name (buffer-file-name))))


(defun k-delete-file-and-buffer ()
  "Kill the current buffer and delete the file."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (when (y-or-n-p "Delete file and buffer?")
	(delete-file filename)
	(message"Deleted file %s" filename)
	(kill-buffer)))))


(defun k-replace-in-buffer ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (call-interactively 'replace-string)))



(defun eye/open-thunar ()
  "Open thunar of current buffer directory."
  (when (and default-directory (executable-find "thunar"))
    (start-process "File manager" nil "thunar" default-directory)))
(defun eye/open-explorer ()
  "Open explorer of current buffer directory."
  (when (and default-directory (file-directory-p default-directory)
	     (eq system-type 'windows-nt))
    (let ((dir default-directory)
	  (explorer (replace-regexp-in-string "/" "\\\\" (executable-find "C:/Windows/SysWOW64/explorer")))
	  (command))
      (setq dir (encode-coding-string
		 (replace-regexp-in-string "/" "\\\\" dir) 'gbk-dos))
      (setq command (concat explorer " " dir))
      (shell-command command nil nil)
      (message command))
    ))
(defun k-open-file-manager ()
  "Open external file manager."
  (interactive)
  (cond ((eq system-type 'windows-nt)
	 (eye/open-explorer))
	((eq system-type 'gnu/linux)
	 (eye/open-thunar))
	(t (message "Not support current system."))
	))



;; 使用 emacsclient 需要先启动服务
(require 'server)
(setq server-name "server") ;;避免和gui版冲突
(if (not (equal t (server-running-p)))
    (server-start))


;;;; keys
;;(add-to-list 'load-path (concat user-emacs-directory "lisp/simple-fly-keys"))
;;(require 'simple-fly-keys)
;;(define-key global-map (kbd "J") #'simple-fly-command-mode-activate)
;;(define-key global-map (kbd "C-j") (lambda () (interactive) (insert "J")))
;;(define-key global-map (kbd "<f1>") #'simple-key-help)
;;(simple-fly-keys 1)

(if (file-exists-p "~/src/emacs-packages/emacs-which-key")
    (progn
      (add-to-list 'load-path "~/src/emacs-packages/emacs-which-key")
      (require 'which-key)
      (which-key-mode 1)))

;; running on msys2, can't use C-c, it is <pause>
(when is-terminal (define-key global-map (kbd "C-x <pause>") 'kill-emacs))


(defalias 'backward-kill-word 'eye/kill-inner-word)

(define-key global-map (kbd "<M-backspace>") 'eye/kill-inner-word)
(define-key global-map (kbd "<C-backspace>") 'eye/kill-inner-word)


;; use [ESC] replace [C-g]
;; 终端下不替换，否则alt+x失效，alt是ESC
(when is-gui
  (define-key key-translation-map (kbd "ESC") (kbd "C-g")))

(define-key global-map (kbd "C-x C-b") #'switch-to-buffer)


(provide 'basic-config)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
