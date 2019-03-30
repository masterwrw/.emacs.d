;;;; Startup
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


(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))
(add-subdirs-to-load-path (concat user-emacs-directory "packages/"))
(add-subdirs-to-load-path (concat user-emacs-directory "modules/"))

(setq custom-file (concat user-emacs-directory "custom.el"))
;;(load custom-file)

;;;; locale
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

;; 防止退出时卡死在 Saving clipboard to X clipboard manager 状态
(setq x-select-enable-clipboard-manager nil)

;;;; System env
(setq is-windows (or
		  (eq system-type 'windows-nt)
		  (eq system-type 'cygwin)))
(setq is-linux (eq system-type 'gnu/linux))
(setq is-mac (eq system-type 'darwin))

(setq is-gui (display-graphic-p))
(setq is-terminal (not (display-graphic-p)))

;;;; UI
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
  (when (eq system-type 'windows-nt)
    (w32-send-sys-command 61488)))

(add-hook 'after-init-hook 'maximize-frame)

;; 不要自动分割窗口 @see https://github.com/ecxr/handmadehero/blob/master/misc/.emacs
;; (setq split-window-preferred-function nil)

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
(when (not (featurep 'x))
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

;; vs2015默认文件编码
(defun eye/convert-to-chinese-iso-8bit-dos ()
  (interactive)
  (set-buffer-file-coding-system 'chinese-iso-8bit-dos 't))

;; org笔记编码
(defun eye/convert-to-utf-8-withsignature-unix ()
  (interactive)
  (set-buffer-file-coding-system 'utf-8-with-signature-unix 't))

;;;; Backup
(require 'f)
(defvar user-cache-directory "~/tmp/emacs_cache")
(unless (f-directory? "~/tmp")
  (make-directory "~/tmp"))
(unless (f-directory? user-cache-directory)
  (make-directory user-cache-directory))
(unless (f-directory? (concat user-cache-directory "/bak"))
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

;;(require 'auto-save)
;;(auto-save-enable)
;;(setq auto-save-silent t)
;; (setq auto-save-delete-trailing-whitespace t)

(require 'super-save)
(super-save-mode +1)
(setq super-save-remote-files nil)

;;;; History
(require 'saveplace)
(save-place-mode 1)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 200)
;;(add-to-list 'recentf-exclude (expand-file-name package-user-dir))
(add-to-list 'recentf-exclude ".cache")
(add-to-list 'recentf-exclude ".cask")
(add-to-list 'recentf-exclude "bookmarks")
(add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")

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

;;;; External
;; 使用 emacsclient 需要先启动服务
(require 'server)
(unless (server-running-p) (server-start))
;; Copy from prelude config
;; https://github.com/bbatsov/prelude/blob/master/core/prelude-core.el
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


;; set modeline face
(setq awesome-tray-mode-line-active-color "color-24")
(setq awesome-tray-mode-line-inactive-color "color-238")
(defvar awesome-tray-mode-line-colors nil)
(defun awesome-tray-enable ()
  ;; Save mode-line colors when first time.
  ;; Don't change `awesome-tray-mode-line-colors' anymore.
  (interactive)
  (unless awesome-tray-mode-line-colors
    (setq awesome-tray-mode-line-colors
          (list (face-attribute 'mode-line :foreground)
                (face-attribute 'mode-line :background)
                (face-attribute 'mode-line :family)
                (face-attribute 'mode-line :box)
                (face-attribute 'mode-line-inactive :foreground)
                (face-attribute 'mode-line-inactive :background)
                (face-attribute 'mode-line-inactive :family)
                (face-attribute 'mode-line-inactive :box)
                )))
  ;; Disable mode line.
  (set-face-attribute 'mode-line nil
                      :foreground awesome-tray-mode-line-active-color
                      :background awesome-tray-mode-line-active-color
                      :height 0.1
                      :box nil)
  (set-face-attribute 'mode-line-inactive nil
                      :foreground awesome-tray-mode-line-inactive-color
                      :background awesome-tray-mode-line-inactive-color
                      :height 0.1
                      :box nil
                      :inherit 'unspecified))
(add-hook 'after-init-hook 'awesome-tray-enable)


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

(setq ivy-format-function 'maple/ivy-format-function)


;; fixed minibuffe
(setq resize-mini-windows nil)
(setq ivy-height 6)

(defun eye/set-mini-window-height (&optional frame)
  (interactive)
  (let ((mini-win (minibuffer-window frame)))
	(when (and mini-win (< (window-size mini-win) ivy-height))
	  (window-resize mini-win ivy-height))))

;; (add-hook 'window-setup-hook 'eye/set-mini-window-height)
;; (add-hook 'after-make-frame-functions 'eye/set-mini-window-height)
;; (add-hook 'move-frame-functions 'eye/set-mini-window-height)
;; no need above hook if use code below
(defun eye/mini-ivy-on ()
  (interactive)
  (add-hook 'window-size-change-functions 'eye/set-mini-window-height)
  (add-hook 'after-init-hook 'eye/set-mini-window-height))
(defun eye/mini-ivy-off ()
  (interactive)
  (remove-hook 'window-size-change-functions 'eye/set-mini-window-height)
  (remove-hook 'after-init-hook 'eye/set-mini-window-height))
			
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

(defun eye/eshell-clear ()
  "Clear eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(add-hook 'eshell-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-l") 'eye/eshell-clear)))


(defun eye/shell-cmd (buffer env)
  "Run cmd with new buffer name and path environment."
  (let ((explicit-shell-file-name "C:\\Windows\\System32\\cmd.exe")
        (shell-path-bak (getenv "PATH")) ;; save path
        (shell-buffer-name buffer)
        (shell-path-cmd env))
    (setenv "PATH" (concat shell-path-cmd "C:\\Windows\\System32;"))
    (shell shell-buffer-name)
    ;; restore path
    (setenv "PATH" shell-path-bak)))

;;;; Buffer
(setq electric-pair-pairs '(
                                                        (?\{ . ?\})
                                                        (?\( . ?\))
                                                        (?\[ . ?\])
                                                        (?\" . ?\")
                                                        ))
(electric-pair-mode t)
(show-paren-mode 1)
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

;; enable narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; save clipboard contents into kill-ring before replace theme
(setq save-interprogram-paste-before-kill t)

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

;; Kill buffers without asking
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

(setq ibuffer-expert t) ;;don't ask when delete

;; 按行滚动
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2))) ;; one line at a time
;; (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;; (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;; (setq scroll-step 5) ;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)



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

;; 自动保存书签
(add-hook 'kill-emacs-hook
          '(lambda ()
             (bookmark-save)))


(require 'multiple-cursors)


(delete-selection-mode 1)

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

;; (require 'watch-other-window)
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

;;;; Elisp
(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))

(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)

;; @See http://metasandwich.com/2013/01/19/emacs-config-youre-doing-it-wrong
(defun eye/init-imenu (p)
  (interactive "P")
  (find-file-existing "~/.emacs.d/init.el")
  (widen)
  (counsel-imenu)
  (if p (init-narrow-to-section)))

(defun init-narrow-to-section ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (unless (looking-at "^;;;;")
      (re-search-backward "^;;;;" nil t))
    (push-mark)
    (next-line)
    (re-search-forward "^;;;;" nil t)
    (previous-line)
    (narrow-to-region (region-beginning) (region-end))))


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

;;;; lua
(require 'init-lua)

;;;; sql
(add-hook 'sql-mode-hook 'yas-minor-mode)

;;;; xml
(require 'nxml-mode)

;;;; Navigation
(require 'backward-forward)

;;;; imenu
(require 'idomenu)
(defhydra hydra-imenu (:exit t)
  ("i" idomenu))
(eye-set-leader-mode-key global-map "i" 'hydra-imenu/body)

;; (require 'init-tags)

;;;; orgmode
(eye--reset-time)
(require 'init-orgmode)
(eye-set-basic-keys org-mode-map)
(eye--print-time "init-orgmode")

(defhydra hydra-global-func (:exit t)
  ("a" org-agenda "Agenda")
  ("c" org-capture "Capture"))
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
  (require 'moe-theme)
  (load-theme 'moe-dark t)
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
(require 'init-orgmode)


;;;; session
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

;; use session-restore to restore the desktop manually
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
(if (eq system-type 'windows-nt)
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

(defun eye-define-key (modmap key func)
  (define-key modmap (kbd (concat eye-leader-key " " key)) func))
;; 这里的 list 不能使用 quote 或 ' 因为 define-key 的第一个参数不是一个 symbol
;; list 中的mode map必须是(keymapp m)为t的
(setq eye-key-mode-list
      (list global-map
	    c++-mode-map
	    c++-mode-map
	    org-mode-map
	    org-src-mode-map
	    emacs-lisp-mode-map
	    lisp-interaction-mode-map
	    counsel-describe-map
	    org-agenda-mode-map
	    nxml-mode-map
	    ))
(when is-linux
  (add-to-list 'eye-key-mode-list magit-mode-map))

;;(global-unset-key (kbd "SPC"))
(dolist (modmap eye-key-mode-list)  
  (define-key modmap (kbd "M-j") 'left-char)
  (define-key modmap (kbd "M-l") 'right-char)
  (define-key modmap (kbd "M-u") 'left-word)
  (define-key modmap (kbd "M-o") 'right-word)
  (define-key modmap (kbd "M-i") 'previous-line)
  (define-key modmap (kbd "M-k") 'next-line)
    
  (define-key modmap (kbd "M-h") 'eye/beginning-of-line-or-block)
  (define-key modmap (kbd "M-;") 'xah-end-of-line-or-block)
  (define-key modmap (kbd "M-n") 'scroll-up-command)
  (define-key modmap (kbd "M-p") 'scroll-down-command)
  (define-key modmap (kbd "M-/") 'xah-comment-dwim)
  (define-key modmap (kbd "M-m") 'set-mark-command)
  (define-key modmap (kbd "M-w") 'xah-copy-line-or-region)
  (define-key modmap (kbd "M-q") 'xah-cut-line-or-region)
  (define-key modmap (kbd "M-a") 'yank)
  (define-key modmap (kbd "M-e") 'eye/eno-copy)
  (define-key modmap (kbd "M-'") 'xah-goto-matching-bracket)

  ;; not working in mobaxterm
  ;; (when is-gui
  (define-key modmap (kbd "<M-left>") 'backward-word)
  (define-key modmap (kbd "<M-right>") 'forward-word)
  (define-key modmap (kbd "<M-up>") 'eye/scroll-up)
  (define-key modmap (kbd "<M-down>") 'eye/scroll-down)
  ;; (define-key modmap (kbd "<C-tab>") 'mode-line-other-buffer)
  ;; )


  (define-key modmap (kbd "C-k") '(lambda ()
				    (interactive)
				    (keyboard-escape-quit)
				    (minibuffer-keyboard-quit)))

  (define-key modmap (kbd eye-leader-key) nil)
  (define-key modmap (kbd "M-,") '(lambda () (interactive (insert ","))))
  (define-key modmap (kbd ",,") 'backward-forward-previous-location)
  (define-key modmap (kbd ",.") 'backward-forward-next-location)
  (eye-define-key modmap "a" 'counsel-M-x)

  (define-key modmap (kbd ",=") 'eye/increase-font-size)
  (define-key modmap (kbd ",-") 'eye/decrease-font-size)
  
  
  ;; delete
  (define-key global-map (kbd "M-8") 'backward-delete-char)
  (define-key global-map (kbd "M-9") 'delete-char)
  (eye-define-key modmap "dd" 'delete-line-no-copy)
  (eye-define-key modmap "du" 'delete-inner-word-no-copy)
  (eye-define-key modmap "do" 'delete-forward-word-no-copy) 
  (eye-define-key modmap "d;" 'delete-end-of-line-no-copy)
  (eye-define-key modmap "dh" 'delete-beginning-of-line-no-copy)

  (eye-define-key modmap "di" 'youdao-dictionary-search-from-input)
  (eye-define-key modmap "dp" 'youdao-dictionary-search-at-point)

  ;; edit
  (eye-define-key modmap "ea" 'mark-whole-buffer)
  (eye-define-key modmap "ee" 'xah-extend-selection)
  (eye-define-key modmap "eq" 'xah-select-text-in-quote)
  (eye-define-key modmap "en" 'narrow-to-region)
  (eye-define-key modmap "ew" 'widen)
  
  ;; buffer and file
  (eye-define-key modmap "fa" 'counsel-ibuffer)
  (eye-define-key modmap "fb" 'bookmark-set)
  (eye-define-key modmap "fd" 'dired-jump)
  (eye-define-key modmap "fs" 'save-buffer)
  (eye-define-key modmap "ff" 'xah-open-file-fast)
  (eye-define-key modmap "fo" 'counsel-find-file)
  (eye-define-key modmap "fr" 'counsel-recentf)
  (eye-define-key modmap "fk" 'xah-close-current-buffer)
  (eye-define-key modmap "fg" 'counsel-git) ;查找在git仓库中的文件，注意最好子目录下没有.git目录，否则可能不会显示出文件列表
  (eye-define-key modmap "fp" 'xah-previous-user-buffer)
  (eye-define-key modmap "fn" 'xah-next-user-buffer)
  (eye-define-key modmap "fl" 'bookmark-bmenu-list)
  (eye-define-key modmap "fj" 'bookmark-jump)
  (eye-define-key modmap "fh" 'helm-mini)
  (eye-define-key modmap "fw" 'write-file)
  (eye-define-key modmap "fx" 'recentf-open-files)
  (eye-define-key modmap "fz" 'xah-open-last-closed)

  (eye-define-key modmap "im" 'counsel-semantic-or-imenu)
  ;; (eye-define-key modmap "jd" 'dumb-jump-go)

  (eye-define-key modmap "c" 'ace-jump-char-mode)
  (eye-define-key modmap "v" 'ace-jump-line-mode)
  (eye-define-key modmap "j" 'ace-jump-word-mode)
  (eye-define-key modmap "l" 'avy-goto-char-in-line)
  ;; (eye-define-key modmap "lb" 'ibuffer)
  
  (eye-define-key modmap "nd" 'eye/notes-dired)
  (eye-define-key modmap "nn" 'eye/notes-new)
  (eye-define-key modmap "na" 'eye/notes-create-attachment)
  (eye-define-key modmap "no" 'eye/notes-open-attachment)
  (eye-define-key modmap "ns" 'eye/notes-search-keyword)
  (eye-define-key modmap "nf" 'eye/notes-search-file)
  
  (eye-define-key modmap "oi" 'eye/init-imenu)
  (eye-define-key modmap "os" 'outline-show-entry)
  (eye-define-key modmap "oh" 'outline-hide-entry)
  (eye-define-key modmap "ob" 'eye/outline-hide-body)

  (eye-define-key modmap "p" 'counsel-yank-pop) ;paste

  (eye-define-key modmap "rr" 'replace-rectangle)
  (eye-define-key modmap "rk" 'kill-rectangle)

  ;; search
  (eye-define-key modmap "sa" 'isearch-forward)
  (eye-define-key modmap "ss" 'swiper)
  (eye-define-key modmap "sd" 'query-replace)
  (eye-define-key modmap "sf" 'eye/replace-string-buffer)
  (eye-define-key modmap "sh" 'color-rg-search-input)
  (eye-define-key modmap "sj" 'color-rg-search-project)
  (eye-define-key modmap "sk" 'counsel-rg-marked)
  (eye-define-key modmap "sl" 'counsel-rg)
  
  ;; (eye-define-key modmap "se" 'aweshell-toggle)
  (eye-define-key modmap "swg" 'prelude-google)
  (eye-define-key modmap "swb" 'prelude-bing)
  (eye-define-key modmap "swd" 'prelude-duckduckgo)
  (eye-define-key modmap "swh" 'prelude-github)
  (eye-define-key modmap "swy" 'prelude-youtube)

  (eye-define-key modmap "tf" 'toggle-frame-fullscreen)

  ;; window
  (eye-define-key modmap "wo" 'xah-next-window-or-frame);xah-unsplit-window-or-next-frame
  (eye-define-key modmap "w0" 'delete-window)
  ;;(eye-define-key modmap "ws" 'hydra-watch-other-window-or-frame/body)
  (eye-define-key modmap "w1" 'delete-other-windows)
  (eye-define-key modmap "w3" 'split-window-horizontally)
  (eye-define-key modmap "w4" 'split-window-vertically)
  (eye-define-key modmap "w7" 'eye/new-frame)
  (eye-define-key modmap "w8" 'delete-other-frames)
  (eye-define-key modmap "w9" 'delete-frame)
  (eye-define-key modmap "wm" 'eye/set-mini-window-height)
  
  (eye-define-key modmap "z" 'undo)
  (if is-terminal
      (eye-define-key modmap "TAB" 'mode-line-other-buffer)
    (eye-define-key modmap "<tab>" 'mode-line-other-buffer))

  ;; help
  (eye-define-key modmap "hd" 'find-function)
  (eye-define-key modmap "hl" 'find-library)
  (if (>= emacs-major-version 26)
      (progn
	(require 'helpful)
	(eye-define-key modmap "hv" 'helpful-variable)
	(eye-define-key modmap "hf" 'helpful-function)
	(eye-define-key modmap "hk" 'helpful-key)
	(eye-define-key modmap "hm" 'describe-mode)
	(eye-define-key modmap "hi" 'info))
    (progn
      (eye-define-key modmap "hv" 'describe-variable)
      (eye-define-key modmap "hf" 'describe-function)
      (eye-define-key modmap "hk" 'describe-key)
      (eye-define-key modmap "hm" 'describe-mode)
      (eye-define-key modmap "hi" 'info)))
  ;; other function

  (eye-define-key modmap "xa" 'org-agenda)
  (eye-define-key modmap "xc" 'org-capture)
  (eye-define-key modmap "xg" 'magit-status)
  (eye-define-key modmap "xr" 'read-only-mode)  
  )


(define-key emacs-lisp-mode-map (kbd ",mx") 'eval-last-sexp)
(define-key lisp-interaction-mode-map (kbd ",mx") 'eval-last-sexp)

(define-key org-mode-map (kbd ",ma") 'org-attach)
(define-key org-mode-map (kbd ",mo") 'org-open-at-point)
(define-key org-mode-map (kbd ",ml") 'org-toggle-link-display)
(define-key org-mode-map (kbd ",mi") 'org-move-subtree-up)
(define-key org-mode-map (kbd ",mk") 'org-move-subtree-down)
(define-key org-mode-map (kbd ",mr") 'org-refile)

(define-key c++-mode-map (kbd ",ma") 'counsel-etags-find-tag-at-point)
(define-key c++-mode-map (kbd ",ms") 'counsel-etags-find-tag)
(define-key c++-mode-map (kbd ",md") 'dumb-jump-go)
(define-key c++-mode-map (kbd ",mq") 'dumb-jump-back)
(define-key c++-mode-map (kbd ",mw") 'dumb-jump-go-other-window)

;;;; which key 
(require 'which-key)
(which-key-mode)
(which-key-add-key-based-replacements ",f" " 文件(f)")
(which-key-add-key-based-replacements ",fz" "打开最后关闭的文件")

(which-key-add-key-based-replacements ",w" " 窗口(w)")
(which-key-add-key-based-replacements ",h" " 帮助(h)")
(which-key-add-key-based-replacements ",d" " 删除(d)")
(which-key-add-key-based-replacements ",e" " 选择(e)")
(which-key-add-key-based-replacements ",s" " 搜索和替换(s)")
(which-key-add-key-based-replacements ",o" " 大纲(o)")
(which-key-add-key-based-replacements ",r" " 矩形(r)")
(which-key-add-key-based-replacements ",n" " 笔记(n)")
(which-key-add-key-based-replacements ",m" " 模式(m)")
(which-key-add-key-based-replacements ",x" " 功能x")
