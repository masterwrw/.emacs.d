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
(add-subdirs-to-load-path (concat user-emacs-directory "site-lisp/"))
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
;; (when menu-bar-mode
  ;; (menu-bar-mode -1)) ;; 禁用菜单栏
(if (and (display-graphic-p) scroll-bar-mode)
    (scroll-bar-mode -1) ;; 禁用滚动条 emacs26 -nw will be error
  )

(setq frame-title-format "Editor %b -- %f") ;; 自定义标题栏

;; 去掉窗口边缘和分割窗口时分割条的边缘
;; http://emacsredux.com/blog/2015/01/18/customizing-the-fringes/
;; linux maybe need gdb, and use mouse to set breakpoint on fringe, so only hide fringe on windows.
(when (eq system-type 'windows-nt)
  (set-window-fringes nil 0 0) ;; border side
  (fringe-mode '(0 . 0)) ;; middle of split frame
  )


(blink-cursor-mode -1) ;; 取消光标闪烁
(setq mouse-yank-at-point t) ;; 强制粘贴时粘贴到光标处

;; @see https://stackoverflow.com/questions/2081577/setting-emacs-split-to-horizontal
(setq split-width-threshold nil) ;;不允许自动左右分屏
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
(unless (f-directory? user-cache-directory) (make-directory user-cache-directory))
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
(add-hook 'after-init-hook 'save-place-mode)

(require 'recentf)
(add-hook 'find-file-hook
          (lambda ()
            (unless recentf-mode
              (recentf-mode)
              (recentf-track-opened-file))))

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


(defun eye/open-file-manager ()
  "Open external file manager."
  (interactive)
  (when (and (executable-find "thunar")
             (not (null default-directory)))
    (start-process "File manager" nil "thunar" default-directory)))

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


;;;; Font
(defvar en-font-name "Liberation Mono")
(defvar cn-font-name "Microsoft YaHei")
(setq en-font-size 14)
(setq cn-font-size 11)
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
(define-key ivy-minibuffer-map (kbd "C-i") 'ivy-previous-line)
(define-key ivy-minibuffer-map (kbd "C-k") 'ivy-next-line)
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
(when (and (equal system-type 'windows-nt) (executable-find "git"))
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
  (require 'magit-extension))

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

;;;; Company
(unless (equal system-type 'windows-nt)
  (require 'company)
  (define-key company-active-map (kbd "C-i") 'company-select-previous)
  (define-key company-active-map (kbd "C-k") 'company-select-next)
  (define-key company-active-map (kbd "<tab>") 'company-select-next)
  (define-key company-search-map (kbd "C-i") 'company-select-previous)
  (define-key company-search-map (kbd "C-k") 'company-select-next)
  (add-hook 'after-init-hook #'global-company-mode)

  (require 'company-yasnippet)
  (require 'company-dabbrev)
  (require 'company-css)
  (require 'company-files)
  (require 'desktop)
  (if (>= emacs-major-version 26)
      (progn
	(require 'company-posframe)
	(company-posframe-mode 1)
	;; Let desktop.el not record the company-posframe-mode
	(push '(company-posframe-mode . nil)
              desktop-minor-mode-table)))

  (global-company-mode)

  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2)
  (setq company-show-numbers t)
  (setq company-echo-delay 0)
  (setq company-require-match nil)

  (setq company-dabbrev-code-everywhere t)
  (setq company-dabbrev-minimum-length 2)
  (setq company-dabbrev-other-buffers 'all)
  (setq company-dabbrev-downcase nil)
  ;; make previous/next selection in the popup cycles
  ;; (setq company-selection-wrap-around t)

  (setq company-dabbrev-char-regexp "[\\.0-9a-z-_'/]") ;adjust regexp make `company-dabbrev' search words like `dabbrev-expand'
  (setq company-dabbrev-code-other-buffers 'all) ;search completion from all buffers, not just same mode buffers.

  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)
  ;; bigger popup window
  (setq company-tooltip-limit 20)
  (set-face-attribute 'company-tooltip nil :foreground "magenta")

  ;; backends
  (setq company-backends nil)

  (add-to-list 'company-backends 'company-css)
  (add-to-list 'company-backends 'company-files)
  (add-to-list 'company-backends 'company-etags)
  ;; company-dabbrev config, it is for current buffer string auto complete
  (add-to-list 'company-backends 'company-dabbrev)
  (add-to-list 'company-backends 'company-dabbrev-code)

  ;; Support yas in commpany
  ;; Note: Must be the last to involve all backends
  (defvar company-enable-yas t
    "Enable yasnippet for all backends.")

  (defun company-backend-with-yas (backend)
    (if (or (not company-enable-yas)
            (and (listp backend) (member 'company-yasnippet backend)))
	backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (setq company-backends (mapcar #'company-backend-with-yas company-backends))
  
  ;; (use-package company-statistics
  ;; :ensure t
  ;; :init
  ;; (let ((dir "~/cache"))
  ;; (if (not (file-exists-p dir))
  ;; (make-directory dir))
  ;; (setq company-statistics-file (concat dir "/company-statistics-cache.el")))
  ;; (company-statistics-mode))

  ) ;; end company


;;;; Python
(defun install-python-env ()
  (require 'jinja2-mode)
  ;;    (add-to-list 'auto-mode-alist '("\\.tmpl$" . jinja2-mode)))
  )

(defun eye/shell-python3 ()
  (interactive)
  (eye/shell-cmd "shell-python3" "C:\\Python\\Python36;C:\\Python\\Python36\\Scripts;")
  )

(defun eye/python-help ()
  "Find python online document."
  (interactive)
  (let ((url "https://docs.python.org/3.5/search.html?q="))
    (setq url (concat url (read-string "Query python document: " (eye/current-word))))
    (browse-url-firefox url)))


(add-hook 'python-mode-hook 'yas-minor-mode)

;;;; cpp and qt configuration
(require 'cc-mode)

(add-hook 'c++-mode-hook 'yas-minor-mode)
(add-hook 'c-mode-hook 'yas-minor-mode)

(define-key c++-mode-map (kbd "<M-up>") 'beginning-of-defun)
(define-key c++-mode-map (kbd "<M-down>") 'end-of-defun)

(unless (equal system-type 'windows-nt)
  (require 'company-c-headers)
  (add-hook 'c++-mode
            (lambda ()
              (add-to-list 'company-backends 'company-c-headers)))
  )

(defun set-tab-width-hook ()
  (setq indent-tabs-mode nil)
  (setq default-tab-width 4)
  (setq tab-width 4)
  (setq c-basic-offset 4) ;; tab 缩进量
  (setq c-default-style "k&r") ;; 大括号缩进位置，https://en.wikipedia.org/wiki/Indentation_style
  (setq tab-stop-list ()))
(add-hook 'c-mode-hook 'set-tab-width-hook)
(add-hook 'c++-mode-hook 'set-tab-width-hook)

(defun eye/find-corresponding-file ()
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

(add-hook 'c++-mode-common-hook
          '(lambda ()
             (local-set-key (kbd "C-c f") 'eye/find-correspoinding-file)))

(add-hook 'c-mode-common-hook
          '(lambda ()
             (local-set-key (kbd "C-c f") 'eye/find-correspoinding-file)))


(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))


;; 奇怪问题：在 emacs 中使用 mingw32-make 编译时总是报错无法找到引用，链接出错。
;; 但是在命令行下却又能成功编译。
;; 所以不直接调用 mingw32-make，而是调用 build.bat 批处理文件来进行编译。
(defvar build-script nil)
(if (eq system-type 'windows-nt)
    (setq build-script "build.bat")
  (setq build-script "build.sh")
  )

(setq qt-dir "C:\\Qt\\Qt4.8.7\\bin")
(setq qtcreator-dir "C:\\Qt\\qtcreator-4.6.0\\bin")
(setq gcc-dir "C:\\Qt\\Qt4.8.7\\bin")
(setq vs-env "C:\\Program Files (x86)\\Microsoft Visual Studio 12.0\\VC\\vcvarsall.bat")

(defun eye/set-gcc-env ()
  (let (path)
    (setq path (concat "@echo off\r\n"
                       "set path=%path%;" qt-dir ";" gcc-dir ";" qtcreator-dir ";" "\r\n"))
    path))

(defun eye/set-vs-env ()
  (let (path)
    (setq path (concat "@echo off\r\n"
                       "call \"" vs-env "\"" "\r\n"))
    path))

(defun eye/get-directory ()
  (let ((dir (read-directory-name "Project Directory: ")))
    (if (not (file-exists-p dir))
        (mkdir dir))
    dir))

(defun eye/create-qt-gcc-build-script ()
  (interactive)
  (let (dir file script command)
    (setq dir (eye/get-directory))
    (setq file (concat dir build-script))
    (setq command (format "mingw32-make -w -f Makefile.Release -C %s" dir))
    (setq script (concat (eye/set-gcc-env) command))
    (f-write script 'gbk file)
    ))

(defun eye/create-qt-vs-build-script ()
  (interactive)
  (let (dir file script command projectfile)
    (setq projectfile (read-file-name "Project file:"))
    (setq dir (file-name-directory projectfile))
    (setq file (concat dir build-script))
    (setq command (format "devenv \"%s\" /build" projectfile))
    (setq script (concat (eye/set-vs-env) command))
    (f-write script 'gbk file)
    ))

(require 'compile)
(setq compilation-directory-locked nil)

;; Compilation
(setq compilation-context-lines 0)
(setq compilation-error-regexp-alist
      (cons '("^\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:fatal error\\|warnin\\(g\\)\\) C[0-9]+:" 2 3 nil (4))
            compilation-error-regexp-alist))

(defun find-project-directory-recursive (x)
  "Recursively search for a makefile."
  (interactive)
  (if (file-exists-p x) t
    (cd "../")
    (find-project-directory-recursive x)))

(defun lock-compilation-directory ()
  "The compilation process should NOT hunt for a makefile"
  (interactive)
  (setq compilation-directory-locked t)
  (message "Compilation directory is locked."))

(defun unlock-compilation-directory ()
  "The compilation process SHOULD hunt for a makefile"
  (interactive)
  (setq compilation-directory-locked nil)
  (message "Compilation directory is roaming."))


(defun find-project-directory ()
  "Find the project directory."
  (interactive)
  (setq find-project-from-directory default-directory)
  ;;(switch-to-buffer-other-window "*compilation*")
  (if compilation-directory-locked (cd last-compilation-directory)
    (cd find-project-from-directory)
    (find-project-directory-recursive build-script)
    (setq last-compilation-directory default-directory)))


;; 在当前和上级目录中查找 Makefile 文件路径
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

;; For M-x compile
(defun build-command ()
  (set (make-local-variable 'compile-command)
       (get-closest-pathname build-script)))

(add-hook 'c++-mode-hook 'build-command)

(defun eye/compile-cpp ()
  (interactive)
  (let (command (get-closest-pathname build-script))
    (compile command))
  )



;; Success or failure of compile
(defun notify-compilation-result(buffer msg)
  "Notify that the compilation is finished."
  (if (string-match "^finished" msg)
      (progn
        ;;    (delete-windows-on buffer) ; Auto close compilation buffer
        (tooltip-show "\n Compilation Successful :-) \n "))
    (tooltip-show "\n Compilation Failed :-( \n ")))

(add-to-list 'compilation-finish-functions 'notify-compilation-result)


(defun make-without-asking ()
  "Make the current build."
  (interactive)
  (if (find-project-directory) (compile (concat "build.bat " (buffer-name (current-buffer)) )))
  ;;(switch-to-buffer-other-window "*compilation*")
  (delete-other-window)
  (switch-to-buffer "*compilation*"))

(defun real-make-without-asking ()
  "Make the current build."
  (interactive)
  (if (find-project-directory) (compile "make" ))
  (switch-to-buffer-other-window "*compilation*")
  (other-window 1))

(define-key c++-mode-map (kbd "<f5>") 'make-without-asking)

(require 'smart-compile)
(setq smart-compile-option-string "-w -s -j4")

(defun eye/cpp-help ()
  "Find cpp reference document."
  (interactive)
  (let ((url "http://zh.cppreference.com/mwiki/index.php?search="))
    (setq url (concat url (read-string "Query cpp document: " (eye/current-word))))
    (browse-url-firefox url)))


(defun eye/shell-cmake ()
  (interactive)
  (eye/shell-cmd "shell-cmake" (concat "C:\\green-soft\\git\\bin;"
                                       "C:\\green-soft\\cmake-3.11.0-rc4-win64-x64\\bin;"
                                       )))


;;;; auto insert
(require 'autoinsert)
(define-auto-insert '(c++-mode . "C++ skeleton")
  '(
    (upcase (concat "_"
                    (replace-regexp-in-string
                     "[^A-Za-z0-9]" "_"
                     (file-name-nondirectory buffer-file-name))))
    "/*******************************************************************************" \n
    "Copyright: WRW.Tec" \n
    "Author: WRW" \n
    "Description: " \n
    "*******************************************************************************/" \n
    "#ifndef " str \n "#define " str "\n\n\n"
    "#endif"
    ))



(defun eye/create-class ()
  "Create a class based qt"
  (interactive)
  (let (class base-class filename)
    (setq class (read-string "Class name: "))  ;; input class name
    (setq base-class (read-string "Based: " "QWidget"))  ;; input base class
    (insert
     (with-temp-buffer
       (if (string-empty-p base-class)
           (insert-file-contents (expand-file-name (concat user-emacs-directory "template/cpp/class.h")))
       (insert-file-contents (expand-file-name (concat user-emacs-directory "template/cpp/class-qt.h"))))
       (beginning-of-buffer)
       (replace-string "name" class)
       (beginning-of-buffer)
       (replace-string "base" base-class)
       (buffer-string)
       ))
    (setq filename (file-name-nondirectory (buffer-file-name)))
    (with-temp-buffer
      (insert
       (with-temp-buffer
         (if (string-empty-p base-class)
             (insert-file-contents (expand-file-name (concat user-emacs-directory "template/cpp/class.h")))
           (insert-file-contents (expand-file-name (concat user-emacs-directory "template/cpp/class-qt.cpp"))))
         (beginning-of-buffer)
         (replace-string "name" class)
         (beginning-of-buffer)
         (replace-string "base" base-class)
         (beginning-of-buffer)
         (replace-string "header" (file-name-sans-extension filename))
         (buffer-string)
         ))
      (write-file (concat (file-name-sans-extension filename) ".cpp")))
    ))

;; Auto generate c++ class implement, function implement, functipn prototype
;;
;; 1.Generate class implement:
;; Move cursor to class name, call srefactor-refactor-at-point,
;; if selecte Other file and cpp file already has content, must open it first,
;; otherwise will be overwritten cpp file.
;;
;; 2.Generate function implement:
;; Move cursor to function name, call srefactor-refactor-at-point, Generate Function Implement
;;
;; 3.Generate function prototype:
;; Move cursor in function, call srefactor-refactor-at-point, Generate Function Prototype
;;
;; 4.Convert function to function pointer
;; Move cursor to function name, call srefactor-refactor-at-point, Generate Function Pointer
;;
;; 5.Extract region to a function:
;; Select a region, call srefactor-refactor-at-point.
;;
;; 6.Rename local variable name:
;; Move cursor on variable name, call srefactor-refactor-at-point
;;

;; (require 'srefactor)
;; (require 'srefactor-lisp)
;; (setq srefactor-ui-menu-show-help nil)

;; (setq semantic-idle-scheduler-idle-time 3)

;; maybe set semanticdb-find-default-throttle, https://emacs-china.org/t/topic/5728/6

;; (add-hook 'c++-mode-hook
                  ;; (lambda ()
                    ;; (semantic-mode 1)
                    ;; (semantic-idle-scheduler-mode 1)
                    ;; (remove-hook 'completion-at-point-functions 'semantic-analyze-completion-at-point-function)
                    ;; (remove-hook 'completion-at-point-functions 'semantic-analyze-notc-completion-at-point-function)
                    ;; (remove-hook 'completion-at-point-functions 'semantic-analyze-nolongprefix-completion-at-point-function)))

;; (define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
;; (define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
;;;; yasnippet
(require 'yasnippet)

;; yankpad
(require 'yankpad)
(setq yankpad-file (concat locale-notebook-dir "/yankpad.org"))
(add-to-list 'hippie-expand-try-functions-list #'yankpad-expand)

;;;; Qt
(require 'qt-pro-mode)
;;  :mode ("\\.pro\\'" "\\.pri\\'")
(add-hook 'qt-pro-mode 'yas-minor-mode)

(require 'css-mode)
;;  (add-to-list 'auto-mode-alist '("\\.qss$" . css-mode)))

(require 'qml-mode)
;;  (autoload 'qml-mode "qml-mode" "Editing Qt Declarative." t)
;;(add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))
(unless (equal system-type 'windows-nt)
  (require 'company-qml)
  (add-hook 'qml-mode
            '(lambda ()
               (require 'company-qml)
               (add-to-list 'company-backends 'company-qml)))
  )


(defun eye/qt5-help ()
  "Find Qt5 document."
  (interactive)
  (let ((url "http://doc.qt.io/qt-5/search-results.html?q="))
    (setq url (concat url (read-string "Query Qt5 document: " (eye/current-word))))
    (browse-url-firefox url)))


(defun qt-mode-style-setup ()
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
    ))

(dolist (hook (list
               'c-mode-hook
               'c++-mode-hook
               'c-mode-common-hook
               ))
  (add-hook hook
	    '(lambda ()
	       (qt-mode-style-setup)
	       (outline-minor-mode 1)
	       (setq outline-regexp "^class\\|^struct\\|^enum\\|^[a-zA-Z][a-zA-Z0-9 _&\*]+::")
	       )))

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
(defun install-lua-env ()
  (require 'lua-mode)
  (setq lua-indent-level 4))
(add-hook 'lua-mode-hook 'yas-minor-mode)

;; (defun eye/lua-shell ()
;; (interactive)
;; (setq default-directory "d:/projects/lua")
;; (eye/shell-cmd "lua-shell" "c:\\Lua5.1;"))

;;;; sql
(add-hook 'sql-mode-hook 'yas-minor-mode)

;;;; xml
(require 'nxml-mode)

;;;; Navigation
(require 'counsel-etags)
;; Don't ask before rereading the TAGS files if they have changed
(setq tags-revert-without-query t)
;; Don't warn when TAGS files are large
(setq large-file-warning-threshold nil)
;; Setup auto update now
(add-hook 'prog-mode-hook
          (lambda ()
            (add-hook 'after-save-hook
                      'counsel-etags-virtual-update-tags 'append 'local)))

(with-eval-after-load 'counsel-etags
  ;; counsel-etags-ignore-directories does NOT support wildcast
  (add-to-list 'counsel-etags-ignore-directories ".git")
  (add-to-list 'counsel-etags-ignore-directories ".svn")
  (add-to-list 'counsel-etags-ignore-directories ".vs")
  (add-to-list 'counsel-etags-ignore-directories "ipch")
  (add-to-list 'counsel-etags-ignore-directories "Debug")
  (add-to-list 'counsel-etags-ignore-directories "Release")
  (add-to-list 'counsel-etags-ignore-directories "Bin")
  (add-to-list 'counsel-etags-ignore-directories "tmp")
  ;; counsel-etags-ignore-filenames supports wildcast
  (add-to-list 'counsel-etags-ignore-filenames "TAGS")
  (add-to-list 'counsel-etags-ignore-filenames "GPATH")
  (add-to-list 'counsel-etags-ignore-filenames "GRTAGS")
  (add-to-list 'counsel-etags-ignore-filenames "GTAGS")
  (add-to-list 'counsel-etags-ignore-filenames "*.json")
  (add-to-list 'counsel-etags-ignore-filenames "ui_*.h")
  (add-to-list 'counsel-etags-ignore-filenames "*.ui")
  (add-to-list 'counsel-etags-ignore-filenames "moc_*.cpp")
  (add-to-list 'counsel-etags-ignore-filenames "*.rc")
  (add-to-list 'counsel-etags-ignore-filenames "*.qrc")
  (add-to-list 'counsel-etags-ignore-filenames "*.tlog")
  (add-to-list 'counsel-etags-ignore-filenames "*.md")
  (add-to-list 'counsel-etags-ignore-filenames "*.bat")
  (add-to-list 'counsel-etags-ignore-filenames "*.txt")
  (add-to-list 'counsel-etags-ignore-filenames "*.pdb")
  (add-to-list 'counsel-etags-ignore-filenames "*.filters")
  (add-to-list 'counsel-etags-ignore-filenames "*.user")
  (add-to-list 'counsel-etags-ignore-filenames "*.vcproj")
  (add-to-list 'counsel-etags-ignore-filenames "*.vcxproj")
  (add-to-list 'counsel-etags-ignore-filenames "*.db")
  (add-to-list 'counsel-etags-ignore-filenames "*.opendb")
  (add-to-list 'counsel-etags-ignore-filenames "*.htm")
  (add-to-list 'counsel-etags-ignore-filenames "*.user")
  (add-to-list 'counsel-etags-ignore-filenames "*.make")
  (add-to-list 'counsel-etags-ignore-filenames "*.sln")
  (add-to-list 'counsel-etags-ignore-filenames "*.exp")
  )

(defun eye/create-ctags-file ()
  "Create ctags file"
  (interactive)
  (let ((command (read-string "command: " "ctags -V -R")))
    (async-shell-command command)
    ))

;; You can change callback counsel-etags-update-tags-backend to update tags file using your own solution,
;;(setq counsel-etags-update-tags-backend (lambda () (shell-command "find . -type f -iname \"*.[ch]\" | etags -")))

(require 'backward-forward)
'(advice-add 'counsel-etags-find-tag-at-point :before #'backward-forward-push-mark-wrapper)
(backward-forward-mode t)

(require 'dumb-jump)
(advice-add 'dumb-jump-go :before #'backward-forward-push-mark-wrapper)


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
(require 'helm-org)

(require 'org)
(setq org-ellipsis " ")
(setq org-src-fontify-natively nil) ;; 代码块内语法高亮
(setq org-src-tab-acts-natively t)
;; (setq org-src-window-setup 'current-window) ;; 在当前window打开
;; (add-hook 'org-mode-hook 'yas-minor-mode)
;; indent content
(setq org-edit-src-content-indentation 0) ;; 默认不缩进
(setq org-startup-indented nil) ;; 是否自动开启org-indent-mode
(setq org-startup-folded (quote overview))
;; hides blank lines between headings
(setq org-cycle-separator-lines 0)
;; always require new line in header below
;;(setq require-final-newline t)
(setq calendar-week-start-day 1) ;; 日历从周一开始显示
(setq org-support-shift-select 1) ;; 是否支持shift+方向键选择
(setq org-hide-emphasis-markers t) ;; 隐藏斜体标记/text/，如果要删除，则确保光标移到斜体文字最后
;; 用圆形符号表示列表开头，匹配" - "
(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))


;; Exported to HTML
(require 'htmlize)


;; Line wrapping
(add-hook 'org-mode-hook
          '(lambda ()
             (visual-line-mode 1)))

(global-set-key (kbd "C-c '") 'org-edit-src-code)

;; 快速添加 src block，使用 <el 加 tab 键
;; emacs-lisp
(add-to-list 'org-structure-template-alist
             '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))

;; c++
(add-to-list 'org-structure-template-alist
             '("cpp" "#+BEGIN_SRC C++\n?\n#+END_SRC"))

;; lua
(add-to-list 'org-structure-template-alist
             '("lu" "#+BEGIN_SRC lua\n?\n#+END_SRC"))

;; python
(add-to-list 'org-structure-template-alist
             '("py" "#+BEGIN_SRC python\n?\n#+END_SRC"))

;; 交互式选择插入代码块 @See http://wenshanren.org/?p=327
(defun eye/org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("C++" "emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
                                        ;(newline-and-indent) ; no auto indent space
    (insert (format "#+BEGIN_SRC %s\n" src-code-type)) ; use lower string
                                        ;(newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))

;; Advise set auto-save-default to nil
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote("crypt")))
(setq org-crypt-key nil);(setq org-crypt-tag-matcher "secret") ;; Custom tag for crypt

;;;; gtd
(require 'org-agenda)
(require 'org-capture)
(require 'find-lisp)

;; full frame show
;; (setq org-agenda-window-setup 'only-window)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAITTING(w)" "SOMEDAY(s)" "|" "DONE(d@/!)" "ABORT(a@/!)")
        (sequence "REPORT(r)" "BUG(b)" "|" "FIXED(f)")
        ))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "#9ff048" :weight bold)
              ("NEXT" :foreground "#ee5555" :weight bold)
              ("WAITING" :foreground "#999933" :weight bold)
              ("SOMEDAY" :foreground "#6e6e6e" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("ABORT" :foreground "#000000" :weight bold))))


(require 'org-protocol)

(defun transform-square-brackets-to-round-ones(string-to-transform)
  "Transforms [ into ( and ] into ), other chars left unchanged."
  (concat 
   (mapcar #'(lambda (c) (if (equal c ?[) ?\( (if (equal c ?]) ?\) c))) string-to-transform))
  )

(setq locale-gtd-inbox (concat locale-gtd-dir "/inbox.org"))
(setq locale-gtd-task (concat locale-gtd-dir "/task.org"))
(setq locale-gtd-finished (concat locale-gtd-dir "/finished.org"))
(setq locale-gtd-trash (concat locale-gtd-dir "/trash.org"))
(setq locale-gtd-someday (concat locale-gtd-dir "/someday.org"))

(setq org-agenda-files (list locale-gtd-task))
(setq org-default-notes-file locale-gtd-inbox)
(defun eye/open-inbox () (interactive) (find-file org-default-notes-file))

;; capture 的目标路径不能直接使用 concat
(setq org-capture-templates
      '(
        ("k"
         "收集" entry (file+headline locale-gtd-inbox "Inbox")
         "* %?\n%i\n"
         :create t)

	("b" "Bookmark" entry (file+headline locale-gtd-inbox "Bookmarks")
         "* %?\n%i\n"
	 :create 1)
        
        ("s"
         "重要紧急任务" entry (file+headline locale-gtd-task "Tasks")
         "* TODO [#A] %?\n%i\n"
         :create t)

        ("d"
         "重要不紧急任务" entry (file+headline locale-gtd-task "Tasks")
         "* TODO [#B] %?\n%i\n"
         :create t)

        ("f"
         "项目任务重要紧急" entry (file+headline locale-gtd-task "Projects")
         "* TODO [#A] %?\n%i\n"
         :create t)

        ("g"
         "项目任务重要不紧急" entry (file+headline locale-gtd-task "Projects")
         "* TODO [#B] %?\n%i\n"
         :create t)

        ;; org-protocol: https://github.com/sprig/org-capture-extension

        ("p" 
         "收集网页内容（自动调用）" entry (file+headline locale-gtd-inbox "Inbox")
         "* [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]] \
                %^G\n:PROPERTIES:\n:Created: %U\n:END:\n\n%i\n%?"
         :create t)
        
        ("L" 
         "收集网页链接（自动调用）" entry (file+headline locale-gtd-inbox "Bookmarks")
         "* [[%:link][%:description]]\n%?\n"
         :create t)
        
        ))


(setq org-refile-targets
      '(
        (locale-gtd-inbox :level . 1)
        (locale-gtd-task :level . 1)
        (locale-gtd-finished :level . 1)
        (locale-gtd-trash :level . 1)
        (locale-gtd-someday :level . 1)
        ))

(setq org-archive-location (concat locale-gtd-finished "::"))


(defalias 'org-beginning-of-line 'eye/beginniing-of-line)


;;;; Notebook
(defun eye/notes-search-keyword ()
  (interactive)
  (let ((keyword (read-string "Search keyword: " (eye/current-word))))
    (counsel-rg keyword locale-notebook-dir)
    ))

(defun eye/notes-search-file ()
  (interactive)
  (let ((keyword (read-string "Search file: " (eye/current-word))))
    (dired locale-notebook-dir)
    (swiper keyword)
    ))

(defun eye/notes-dired ()
  (interactive)
  (dired locale-notebook-dir))

(defun eye/notes-new ()
  (interactive)
  (let ((name (read-string "New note(no suffix): ")))
    (find-file (concat locale-notebook-dir "/" name ".org"))
    (set-buffer-file-coding-system 'utf-8-with-signature-unix 't) ;; 设置编码
    (insert (concat "* " name)) ;; 添加一级标题
    ))

(defun eye/notes-create-attachment ()
  "创建文件对应的附件文件夹"
  (interactive)
  (let* ((name (replace-regexp-in-string ".org" "" (buffer-name)))
	 (dir (concat locale-notebook-attachment-dir "/" name)))
    (unless (f-directory? locale-notebook-attachment-dir) (f-mkdir locale-notebook-attachment-dir)) ;; 创建附件主目录
    (unless (f-directory? dir) ;; 创建附件子目录
      (f-mkdir dir))
    ))

(defun eye/notes-open-attachment ()
  "打开与当前文件名相同的附件文件夹"
  (interactive)
  (let* ((name (replace-regexp-in-string ".org" "" (buffer-name)))
	 (dir (concat locale-notebook-attachment-dir "/" name)))
    (if (f-directory? dir)
	(progn
	  (shell-command (concat "explorer "
				 (encode-coding-string
				  (replace-regexp-in-string "/" "\\\\" dir) 'gbk-dos))) ;; 转换为windows路径后再转换为gbk编码，否则无法打开中文目录
	  )
      (message "Attachment folder not exists!")	
      )))


;;;; session
(require 'base-toolkit)

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

(require 'auto-save)
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
(add-hook 'after-init-hook
	  '(lambda ()
	     (if (saved-session)
		 (if (y-or-n-p "Restore desktop? ")
		     (session-restore)))))

(add-hook 'kill-emacs-hook 'session-save)

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
(require 'which-key)
(which-key-mode)

(set-cursor-color "#00A876")

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

(define-key org-src-mode-map (kbd "C-s") 'org-edit-src-save)

(define-key global-map (kbd "<C-wheel-up>") 'eye/increase-font-size)
(define-key global-map (kbd "<C-wheel-down>") 'eye/decrease-font-size)

(if (>= emacs-major-version 26)
    (progn
      (require 'helpful)
      (define-key global-map (kbd "C-h v") 'helpful-variable)
      (define-key global-map (kbd "C-h f") 'helpful-function)
      (define-key global-map (kbd "C-h k") 'helpful-key)
      (define-key global-map (kbd "C-h m") 'describe-mode)
      (define-key global-map (kbd "C-h i") 'info))
  (progn
    (define-key global-map (kbd "C-h v") 'describe-variable)
    (define-key global-map (kbd "C-h f") 'describe-function)
    (define-key global-map (kbd "C-h k") 'describe-key)
    (define-key global-map (kbd "C-h m") 'describe-mode)
    (define-key global-map (kbd "C-h i") 'info)))
(define-key global-map (kbd "C-h d") 'find-function)
(define-key global-map (kbd "C-h l") 'find-library)

(global-set-key (kbd "<f3> u") 'winner-undo)
(global-set-key (kbd "<f3> i") 'winner-redo)
(global-set-key (kbd "<f3> n") 'new-frame)
(global-set-key (kbd "<f3> k") 'delete-frame)


(define-key emacs-lisp-mode-map (kbd "<f5>") 'eval-last-sexp)
(define-key lisp-interaction-mode-map (kbd "<f5>") 'eval-last-sexp)

(define-key global-map (kbd "<f7> c") 'org-capture)
(define-key global-map (kbd "<f7> a") 'org-agenda)
(define-key global-map (kbd "<f7> r") 'aweshell-toggle)
(define-key global-map (kbd "<f7> g") 'magit-status)
(define-key global-map (kbd "<f7> ss") 'prelude-google)
(define-key global-map (kbd "<f7> sb") 'prelude-bing)
(define-key global-map (kbd "<f7> sd") 'prelude-duckduckgo)
(define-key global-map (kbd "<f7> sg") 'prelude-github)
(define-key global-map (kbd "<f7> sv") 'prelude-youtube)
(define-key global-map (kbd "<f7> qq") 'youdao-dictionary-search-from-input)
(define-key global-map (kbd "<f7> qw") 'youdao-dictionary-search-at-point)
(define-key global-map (kbd "<f9> s") 'outline-show-entry)
(define-key global-map (kbd "<f9> h") 'outline-hide-entry)
(define-key global-map (kbd "<f9> a") 'outline-hide-body)

;; 这里的 list 不能使用 quote 或 ' 因为 define-key 的第一个参数不是一个 symbol
(dolist (modmap (list global-map c++-mode-map org-mode-map
		      org-src-mode-map nxml-mode-map emacs-lisp-mode-map lisp-interaction-mode-map))
  (progn
    (define-key modmap (kbd "M-j") 'left-char)
    (define-key modmap (kbd "M-l") 'right-char)
    (define-key modmap (kbd "M-u") 'left-word)
    (define-key modmap (kbd "M-o") 'right-word)
    (define-key modmap (kbd "M-i") 'previous-line)
    (define-key modmap (kbd "M-k") 'next-line)
    (define-key modmap (kbd "M-h") 'eye/beginning-of-line-or-block)
    (define-key modmap (kbd "M-;") 'xah-end-of-line-or-block)
    (define-key modmap (kbd "M-n") 'eye/scroll-down)
    (define-key modmap (kbd "M-p") 'eye/scroll-up)
    (define-key modmap (kbd "M-/") 'xah-comment-dwim)
    (define-key modmap (kbd "M-m") 'set-mark-command)
    (define-key modmap (kbd "M-w") 'xah-copy-line-or-region)
    (define-key modmap (kbd "M-q") 'xah-cut-line-or-region)
    (define-key modmap (kbd "M-a") 'yank)
    (define-key modmap (kbd "<M-left>") 'backward-word)
    (define-key modmap (kbd "<M-right>") 'forward-word)
    (define-key modmap (kbd "<M-up>") 'eye/scroll-up)
    (define-key modmap (kbd "<M-down>") 'eye/scroll-down)
    (define-key modmap (kbd "M-,") 'backward-forward-previous-location)
    (define-key modmap (kbd "M-.") 'backward-forward-next-location)
    (define-key modmap (kbd "C-k") 'nil)
    (define-key modmap (kbd "C-,") 'keyboard-escape-quit)
    (define-key modmap (kbd "C-k C-,") 'keyboard-escape-quit)
    (define-key modmap (kbd "C-s") 'save-buffer)
    (define-key modmap (kbd "<C-tab>") 'mode-line-other-buffer)
    (define-key modmap (kbd "C-l") 'recenter-top-bottom)

    (define-key modmap (kbd "C-k a") 'counsel-M-x)
    (define-key modmap (kbd "C-k m") 'counsel-ibuffer)
    (define-key modmap (kbd "C-k ff") 'counsel-find-file)
    (define-key modmap (kbd "C-k fo") 'find-file-other-window)
    (define-key modmap (kbd "C-k fk") 'kill-this-buffer)
    (define-key modmap (kbd "C-k fd") 'dired-jump)
    (define-key modmap (kbd "C-k q") 'swiper)
    (define-key modmap (kbd "C-k rq") 'query-replace)
    (define-key modmap (kbd "C-k rr") 'replace-rectangle)
    (define-key modmap (kbd "C-k rk") 'kill-rectangle)
    (define-key modmap (kbd "C-k v") 'counsel-yank-pop)
    (define-key modmap (kbd "C-k c") 'eye/eno-copy)

    (define-key modmap (kbd "C-k wh") 'split-window-horizontally)
    (define-key modmap (kbd "C-k wv") 'split-window-vertically)
    (define-key modmap (kbd "C-k ww") 'xah-next-window-or-frame)
    (define-key modmap (kbd "C-k wo") 'delete-other-windows)
    (define-key modmap (kbd "C-k wk") 'delete-window)
    
    (define-key modmap (kbd "C-k 8") 'xah-extend-selection)
    (define-key modmap (kbd "C-k s") 'save-buffer)
    (define-key modmap (kbd "C-k z") 'undo)
    (define-key modmap (kbd "C-k <tab>") 'mode-line-other-buffer)
    (define-key modmap (kbd "C-k dd") 'delete-line-no-copy)
    (define-key modmap (kbd "C-k dl") 'delete-char)
    (define-key modmap (kbd "C-k du") 'delete-inner-word-no-copy)
    (define-key modmap (kbd "C-k do") 'delete-forward-word-no-copy) 
    (define-key modmap (kbd "C-k d;") 'delete-end-of-line-no-copy)
    (define-key modmap (kbd "C-k dh") 'delete-beginning-of-line-no-copy)
    (define-key modmap (kbd "C-k dj") 'delete-backward-char)
    (define-key modmap (kbd "C-k dw") 'delete-window)
    (define-key modmap (kbd "C-k jc") 'avy-goto-char)
    (define-key modmap (kbd "C-k jl") 'avy-goto-line)
    (define-key modmap (kbd "C-k jd") 'dumb-jump-go) ;; jump to define
    (define-key modmap (kbd "C-k i") 'counsel-semantic-or-imenu)
    (define-key modmap (kbd "C-k bl") 'bookmark-bmenu-list)
    (define-key modmap (kbd "C-k bs") 'bookmark-set)
    (define-key modmap (kbd "C-k bj") 'bookmark-jump)
    (define-key modmap (kbd "C-k ei") 'eye/init-imenu)

    (define-key modmap (kbd "C-.") nil)
    (define-key modmap (kbd "C-. nd") 'eye/notes-dired)
    (define-key modmap (kbd "C-. nn") 'eye/notes-new)
    (define-key modmap (kbd "C-. na") 'eye/notes-create-attachment)
    (define-key modmap (kbd "C-. no") 'eye/notes-open-attachment)
    (define-key modmap (kbd "C-. ns") 'eye/notes-search-keyword)
    (define-key modmap (kbd "C-. nf") 'eye/notes-search-file)

    (define-key modmap (kbd "C-. ym") 'yankpad-map)
    (define-key modmap (kbd "C-. yi") 'yankpad-insert)
    ))
