;;; configuration.el --- My emacs configuration -*- lexical-binding: t -*-

;; Produce backtraces when errors occur
(setq debug-on-error t)

(let ((minver "24.4"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "25.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))


(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

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

(defvar begin-time (current-time))
(defun eye--reset-time ()
  (setq begin-time (current-time)))

(defun eye--print-time (msg)
  "Print cost time from begin-time.
 use eye--reset-time to reset begin-time."
  (message
   (format "-----------------------------------------------%.6f sec: %s"
	   (- (float-time (current-time))
	      (float-time begin-time))
	   msg)))

(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))

(add-subdirs-to-load-path "~/packages/") ;; all site packages don't put to .emacs.d
(add-subdirs-to-load-path (concat user-emacs-directory "lisp/"))

(eye--print-time "add path cost")

(setq custom-file (concat user-emacs-directory "custom.el"))
;;(load custom-file)

;;;; system env
(setq is-windows (or
		  (eq system-type 'windows-nt)
		  (eq system-type 'cygwin)))
(setq is-linux (eq system-type 'gnu/linux))
(setq is-mac (eq system-type 'darwin))

(setq is-gui (display-graphic-p))
(setq is-terminal (not (display-graphic-p)))

;;;; misc
;; 防止退出时卡死在 Saving clipboard to X clipboard manager 状态
(setq x-select-enable-clipboard-manager nil)

(setq inhibit-startup-message t) ;; 禁用启动后显示的消息 buffer
(setq initial-scratch-message nil) ;; 禁止显示 *scratch* buffer 中默认出现的文本
(put 'suspend-frame 'disabled t) ;; 禁止 Ctrl+z 时挂起 emacs

;; 用 y/n 代替 yes/no 的回答
(defalias 'yes-or-no-p 'y-or-n-p) ;; (fset 'yes-or-no-p 'y-or-n-p) 相同的效果

(setq ring-bell-function 'ignore) ;; 禁止出现烦人的响铃

;; Fix load slow, https://github.com/raxod502/radian/issues/180
(when tool-bar-mode
  (tool-bar-mode -1)) ;; 禁用工具栏
(when (and is-terminal menu-bar-mode)
  (menu-bar-mode -1)) ;; 禁用菜单栏
(when (and is-gui scroll-bar-mode)
  (scroll-bar-mode -1)) ;; 禁用滚动条 emacs26 -nw will be error

(setq frame-title-format "Editor %b -- %f") ;; 自定义标题栏

;; 去掉窗口边缘和分割窗口时分割条的边缘
;; http://emacsredux.com/blog/2015/01/18/customizing-the-fringes/
;; linux maybe need gdb, and use mouse to set breakpoint on fringe, so only hide fringe on windows.
(when (and is-windows is-gui)
  (set-window-fringes nil 5 3) ;; border side
  (fringe-mode '(0 . 0)) ;; middle of split frame
  )


(blink-cursor-mode -1) ;; 取消光标闪烁
(when is-gui
  (add-hook 'after-init-hook
	    (lambda ()
	      (set-cursor-color "#00A876"))))

(setq mouse-yank-at-point t) ;; 强制粘贴时粘贴到光标处

;; @see https://stackoverflow.com/questions/2081577/setting-emacs-split-to-horizontal
;; (setq split-width-threshold nil) ;;不允许自动左右分屏
(setq split-height-threshold nil) ;;不允许自动上下分屏

;; 全屏
(defun fullscreen ()
  "Fullscreen."
  (interactive)
  (set-frame-parameter nil 'fullscreen 'fullboth))

(defun fullscreen-toggle ()
  "Toggle fullscreen status."
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

;; (fullscreen)

;; 最大化
(defun maximize-frame ()
  "Maximizes the active frame in Windows"
  (interactive)
  ;; Send a `WM_SYSCOMMAND' message to the active frame with the
  ;; `SC_MAXIMIZE' parameter.
  (when is-windows
    (w32-send-sys-command 61488)))

;(add-hook 'after-init-hook 'maximize-frame)

;; 不要自动分割窗口 @see https://github.com/ecxr/handmadehero/blob/master/misc/.emacs
;; (setq split-window-preferred-function nil)

;; 按行滚动
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2))) ;; one line at a time
;; (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;; (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;; (setq scroll-step 5) ;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;;;; History
(eye--reset-time)
(require 'saveplace)
(save-place-mode 1)
(eye--print-time "require saveplace")


(eye--reset-time)
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 200)
;;(add-to-list 'recentf-exclude (expand-file-name package-user-dir))
(add-to-list 'recentf-exclude ".cache")
(add-to-list 'recentf-exclude ".cask")
(add-to-list 'recentf-exclude "bookmarks")
(add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
(eye--print-time "require recentf")


;;;; Backup
;;(require 'f)
(defvar user-cache-directory "~/tmp/emacs_cache")
(file-exists-p user-cache-directory)
(unless (file-exists-p "~/tmp")
  (make-directory "~/tmp"))
(unless (file-exists-p user-cache-directory)
  (make-directory user-cache-directory))
(unless (file-exists-p (concat user-cache-directory "/bak"))
  (make-directory (concat user-cache-directory "/bak")))
;; 备份文件 file~，指定备份目录后，文件名为 !drive_f!dirname!dirname!filename~
(setq backup-by-copying t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)
(setq backup-directory-alist '(("." . "~/tmp/emacs_cache/bak")))
;; 临时文件 #file#
(setq auto-save-default t) ;; 开启自动备份临时文件，auto-save.el 中会修改这个变量
(setq auto-save-file-name-transforms '((".*" "~/tmp/emacs_cache/bak" t))) ;; 设置备份文件目录

;;;; Encoding
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
      (set-file-name-coding-system 'gbk)))
  (add-hook 'shell-mode-hook 'eye/change-shell-mode-coding)
  (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))

(defun eye/convert-to-utf8-unix ()
  (interactive)
  (set-buffer-file-coding-system 'unix 't))

;; vs文件编码
(defun eye/convert-to-gbk-dos ()
  (interactive)
  (set-buffer-file-coding-system 'chinese-gbk-dos 't))

;; org笔记编码
(defun eye/convert-to-utf-8-withsignature-unix ()
  (interactive)
  (set-buffer-file-coding-system 'utf-8-with-signature-unix 't))


;;;; modeline
;; Copy from https://github.com/redguardtoo/emacs.d/blob/master/lisp/init-modeline.el
;; @see http://emacs-fu.blogspot.com/2011/08/customizing-mode-line.html
;; But I need global-mode-string,
;; @see http://www.delorie.com/gnu/docs/elisp-manual-21/elisp_360.html
;; use setq-default to set it for /all/ modes
(setq-default mode-line-format
              (list
               ;; the buffer name; the file name as a tool tip
               '(:eval (propertize (if (buffer-modified-p)
                                                                   "%b* "
                                                                 "%b ")
                                                           'face nil
                                   'help-echo (buffer-file-name)))

               ;; line and column
               "(" ;; '%02' to set to 2 chars at least; prevents flickering
               "%02l" "," "%01c"
               ;; (propertize "%02l" 'face 'font-lock-type-face) ","
               ;; (propertize "%02c" 'face 'font-lock-type-face)
               ") "

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

               ;;global-mode-string, org-timer-set-timer in org-mode need this
               (propertize "%M" 'face nil)

               " --"
               ;; i don't want to see minor-modes; but if you want, uncomment this:
               ;; minor-mode-alist  ;; list of minor modes
               "%-" ;; fill with '-'
               ))



;; Show modeline information on top header
;; (setq-default header-line-format mode-line-format) ; Copy mode-line
;; (setq-default mode-line-format nil) ; Remove mode-line
(set-face-attribute 'header-line nil :background "white" :foreground "black")


(setq electric-pair-pairs '(
                                                        (?\{ . ?\})
                                                        (?\( . ?\))
                                                        (?\[ . ?\])
                                                        (?\" . ?\")
                                                        ))
(electric-pair-mode t)
(show-paren-mode 1)



;; enable narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; save clipboard contents into kill-ring before replace theme
(setq save-interprogram-paste-before-kill t)


;; Kill buffers without asking
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

(setq ibuffer-expert t) ;;don't ask when delete



;; 自动保存书签
(add-hook 'kill-emacs-hook
          '(lambda ()
             (bookmark-save)))


(delete-selection-mode 1)

;;;; leader key
(require 'hydra)
(defun unset-leader-key (modmap)
  (define-key modmap (kbd ",") nil)
  (define-key modmap (kbd "M-,") (lambda () (interactive) (insert ","))))

(unset-leader-key global-map)

(defun eye-set-mode-key (modmap keychar func)
  (define-key modmap (kbd keychar) func))

(defun eye-set-leader-mode-key (modmap keychar func)
  (define-key modmap (kbd (concat "," keychar)) func))


;;;; font
(eye--reset-time)
(when is-linux
  (setq en-font-name "DejaVu Sans Mono")
  (setq cn-font-name "YaHei Consolas Hybrid")
  (setq en-font-size 14)
  (setq cn-font-size 14)
  )
(when is-windows
  (setq en-font-name "Liberation Mono")
  (setq cn-font-name "Microsoft YaHei")
  (setq en-font-size 14)
  (setq cn-font-size 11)
  )

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
  )

(eye--print-time "init font")

;;;; locale
(eye--reset-time)
(setq locale-config-file (expand-file-name "locale.el" user-emacs-directory))
(defun eye/load-locale-file()
  "加载本地的一些配置，比如一些路径相关的变量"
  (interactive)
  (if (file-exists-p locale-config-file)
      (load-file locale-config-file)))

(eye/load-locale-file)

(defun eye/open-locale-file ()
  (interactive)
  (find-file locale-config-file))

(eye--print-time "load locale")


;;;; base toolkit
(require 'base-toolkit)

;; 使用 emacsclient 需要先启动服务
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
	      (server-start))))


(eye--reset-time)
(require 'ido)
(ido-mode t)
(eye--print-time "require ido")

;;;; basic keys
(require 'hydra)
(defhydra hydra-help (:exit t :idle 1.0)
  ("v" describe-variable "Desc var")
  ("f" describe-function "Desc fun")
  ("k" describe-key "Desc key")
  ("a" describe-face "Desc face")
  ("i" info "Info"))
(eye-set-leader-mode-key global-map "h" 'hydra-help/body)


;;;; move cursor
(defhydra hydra-move (:column 4 :idle 1.0)
  ("j" left-char)
  ("M-j" left-char)
  ("l" right-char)
  ("M-l" right-char)
  ("u" left-word)
  ("M-u" left-word)
  ("o" right-word)
  ("M-o" right-word)
  ("i" previous-line)
  ("M-i" previous-line)
  ("k" next-line)
  ("M-k" next-line)
  ("h" eye/beginning-of-line-or-block)
  ("M-h" eye/beginning-of-line-or-block)
  (";" xah-end-of-line-or-block)
  ("M-;" xah-end-of-line-or-block)
  ("n" scroll-up-command)
  ("M-n" scroll-up-command)
  ("p" scroll-down-command)
  ("M-p" scroll-down-command)
  ("m" set-mark-command)
  ("b" xah-goto-matching-bracket "goto match bracket")
  ("/" xah-comment-dwim)
  ("SPC" keyboard-quit "quit" :exit t) 		;keyboard-quit to quit mark state
  )

(defun eye-set-basic-keys (modmap)
  (interactive)
  (eye-set-mode-key modmap "M-j" '(lambda () (interactive) (left-char) (hydra-move/body)))
  (eye-set-mode-key modmap "M-l" '(lambda () (interactive) (right-char) (hydra-move/body)))
  (eye-set-mode-key modmap "M-u" '(lambda () (interactive) (left-word) (hydra-move/body)))
  (eye-set-mode-key modmap "M-o" '(lambda () (interactive) (right-word) (hydra-move/body)))
  (eye-set-mode-key modmap "M-i" '(lambda () (interactive) (previous-line) (hydra-move/body)))
  (eye-set-mode-key modmap "M-k" '(lambda () (interactive) (next-line) (hydra-move/body)))
  (eye-set-mode-key modmap "M-h" '(lambda () (interactive) (eye/beginning-of-line-or-block) (hydra-move/body)))
  (eye-set-mode-key modmap "M-;" '(lambda () (interactive) (xah-end-of-line-or-block) (hydra-move/body)))
  (eye-set-mode-key modmap "M-n" '(lambda () (interactive) (scroll-up-command) (hydra-move/body)))
  (eye-set-mode-key modmap "M-p" '(lambda () (interactive) (scroll-down-command) (hydra-move/body)))
  (eye-set-mode-key modmap "M-m" 'set-mark-command)
  (eye-set-mode-key modmap "M-w" 'xah-copy-line-or-region)
  (eye-set-mode-key modmap "M-q" 'xah-cut-line-or-region)
  (eye-set-mode-key modmap "M-a" 'yank)
  (eye-set-mode-key modmap "M-z" 'undo))

(eye-set-basic-keys global-map)

;;;; rectangle
(defhydra hydra-rect (:idle 1.0)
  ("r" replace-rectangle "Replace rectangle" :exit t)
  ("k" kill-rectangle "Kill rectangle" :exit t))
(eye-set-leader-mode-key global-map "r" 'hydra-rect/body)


;;;; jump
(require 'avy)
(require 'ace-jump-mode)
(defhydra hydra-jump (:exit t :idle 1.0)
  ("c" ace-jump-char-mode "Jump char")
  ("l" ace-jump-line-mode "Jump line")
  ("v" avy-goto-char-in-line "Jump inline")
  ("g" goto-line "Goto line")
  ("SPC" keyboard-quit "quit")
  )
(eye-set-leader-mode-key global-map "c" 'hydra-jump/body)


;;;; smex
(require 'smex)
(define-key global-map (kbd ",a") 'smex)

;;;; files
(require 'recentf)
(require 'dired)
(defhydra hydra-file (:exit t :idle 1.0)
  ("a" switch-to-buffer "Switch buffer")
  ("s" save-buffer "Save buffer")
  ("o" ido-find-file "Find file")
  ("h" recentf-open-files)
  ("k" kill-this-buffer "Kill buffer")
  ("d" dired-jump "Dired")
  ("z" xah-open-last-closed "Open last closed")
  ("b" bookmark-set "Set bookmark")
  ("f" xah-open-file-fast "Jump bookmark")
  ("l" bookmark-bmenu-list "List bookmark")
  ("p" xah-previous-user-buffer "Previous buffer")
  ("n" xah-next-user-buffer "Next buffer")
  )
(eye-set-leader-mode-key global-map "f" 'hydra-file/body)

(if is-terminal
    (eye-set-leader-mode-key global-map " TAB" 'mode-line-other-buffer)
  (eye-set-leader-mode-key global-map " <tab>" 'mode-line-other-buffer))

;;;; select
(defhydra hydra-select (:idle 1.0)
  ("SPC" keyboard-quit "quit" :exit t)
  ("a" mark-whole-buffer "Select all" :exit t)
  ("e" xah-extend-selection "Extend")
  ("q" xah-select-text-in-quote "Select quote" :exit t)
  ("l" xah-select-line "Select line" :exit t)
  ("n" narrow-to-region "Narrorw" :exit t)
  ("w" widen "widen" :exit t)
  )
(eye-set-leader-mode-key global-map "e" 'hydra-select/body)


;;;; delete
(define-key global-map (kbd "M-8") 'backward-delete-char)
(define-key global-map (kbd "M-9") 'delete-char)
(defhydra hydra-delete (:exit t :idle 1.0)
  ("d" delete-line-no-copy)
  ("u" delete-inner-word-no-copy)
  ("o" delete-forward-word-no-copy)
  (";" delete-end-of-line-no-copy)
  ("h" delete-beginning-of-line-no-copy))
(eye-set-leader-mode-key global-map "d" 'hydra-delete/body)

;;;; window
(defhydra hydra-window (:idle 1.0)
  ("SPC" nil "quit")
  ("n" eye/new-frame)
  ("o" xah-next-window-or-frame "Next window/frame")
  ("0" delete-window-or-frame "Delete window/frame" :exit t)
  ("1" delete-other-windows "Delete other window" :exit t)
  ("3" split-window-horizontally "Split hor" :exit t)
  ("4" split-window-vertically "Split ver" :exit t))
(eye-set-leader-mode-key global-map "w" 'hydra-window/body)

;;;; search
(defhydra hydra-search (:idle 1.0)
  ("SPC" nil "quit" :exit t)
  ("f" isearch-forward "isearch-forward" :exit t)
  ("b" isearch-backward "isearch-backward" :exit t)
  ("q" query-replace "query-replace" :exit t)
  ("r" eye/replace-string-buffer "Replace all" :exit t)
  )
(define-key isearch-mode-map (kbd "M-k") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "M-i") 'isearch-repeat-backward)
(eye-set-leader-mode-key global-map "s" 'hydra-search/body)


;;;; imenu
(require 'idomenu)
(defhydra hydra-imenu (:exit t :idle 1.0)
  ("i" idomenu))
(eye-set-leader-mode-key global-map "i" 'hydra-imenu/body)

;;;; outline
(defhydra hydra-outline (:exit t :idle 1.0)
  ("s" outline-show-entry)
  ("h" outline-hide-entry)
  ("b" outline-hide-body)
  ("a" outline-show-all))
(eye-set-leader-mode-key global-map "o" 'hydra-outline/body)

;;;; orgmode
(eye--reset-time)
(require 'init-orgmode)
(eye-set-basic-keys org-mode-map)
(eye--print-time "init-orgmode")

(defhydra hydra-global-func (:idle 1.0)
  ("a" org-agenda "Agenda" :exit t)
  ("c" org-capture "Capture" :exit t))

(when is-gui
  (defhydra+ hydra-global-func (:idle 1.0)
    ("=" eye/increase-font-size "Font++")
    ("-" eye/decrease-font-size "Font--")))

(eye-set-leader-mode-key global-map "x" 'hydra-global-func/body)

;;;; elisp
(eye--reset-time)
(require 'init-elisp)
(eye--print-time "init-elisp")


;;;; packages
(setq is-load-packages t)
(when is-load-packages
  ;; theme
  (eye--reset-time)
  ;; (require 'moe-theme)
  ;; (load-theme 'moe-dark t)
  (require 'spolsky-theme)
  (load-theme 'spolsky t)
  (eye--print-time "load theme")

  (eye--reset-time)
  (require 'init-ivy)
  (eye--print-time "init-ivy")

  (eye--reset-time)
  (require 'which-key)
  (which-key-mode)
  (eye--print-time "require whichkey")

  (when is-linux
    (eye--reset-time)
    (require 'init-magit)
    (eye--print-time "init-magit"))

  (eye--reset-time)
  (require 'init-cpp)
  (unset-leader-key c++-mode-map)
  (eye-set-basic-keys c++-mode-map)
  (define-key c++-mode-map (kbd ",f") 'hydra-file/body)
  (define-key c++-mode-map (kbd ",r") 'hydra-rect/body)
  (define-key c++-mode-map (kbd ",c") 'hydra-jump/body)
  (define-key c++-mode-map (kbd ",e") 'hydra-select/body)
  (define-key c++-mode-map (kbd ",d") 'hydra-delete/body)
  (define-key c++-mode-map (kbd ",w") 'hydra-window/body)
  (define-key c++-mode-map (kbd ",s") 'hydra-search/body)
  (define-key c++-mode-map (kbd ",i") 'hydra-imenu/body)
  (define-key c++-mode-map (kbd ",o") 'hydra-outline/body)
  (define-key c++-mode-map (kbd ",x") 'hydra-global-func/body)
  (eye--print-time "init-cpp")


  (require 'init-external)
  
  )








(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
