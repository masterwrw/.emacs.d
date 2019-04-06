;;;; misc
;; 防止退出时卡死在 Saving clipboard to X clipboard manager 状态
(setq x-select-enable-clipboard-manager nil)

(setq inhibit-startup-message t) ;; 禁用启动后显示的消息 buffer
(setq initial-scratch-message nil) ;; 禁止显示 *scratch* buffer 中默认出现的文本
(setq inhibit-compacting-font-caches t) ;; Don’t compact font caches during GC.
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-screen t)
(setq use-file-dialog nil)
(setq use-dialog-box nil)

(put 'suspend-frame 'disabled t) ;; 禁止 Ctrl+z 时挂起 emacs

;; 用 y/n 代替 yes/no 的回答
(defalias 'yes-or-no-p 'y-or-n-p) ;; (fset 'yes-or-no-p 'y-or-n-p) 相同的效果

(setq ring-bell-function 'ignore) ;; 禁止出现烦人的响铃

;; Fix load slow, https://github.com/raxod502/radian/issues/180
(when tool-bar-mode
  (tool-bar-mode -1)) ;; 禁用工具栏
(when menu-bar-mode
  (menu-bar-mode -1)) ;; 禁用菜单栏
(when (and is-gui scroll-bar-mode)
  (scroll-bar-mode -1)) ;; 禁用滚动条 emacs26 -nw will be error

(setq frame-title-format "Editor") ;; 自定义标题栏

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

(setq track-eol t)                      ; Keep cursor at end of lines. Require line-move-visual is nil.
(setq line-move-visual nil)

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


;; 最大化
(defun maximize-frame ()
  "Maximizes the active frame in Windows"
  (interactive)
  ;; Send a `WM_SYSCOMMAND' message to the active frame with the
  ;; `SC_MAXIMIZE' parameter.
  (when is-windows
    (w32-send-sys-command 61488)))



(maximize-frame)


(defhydra+ hydra-funcs (:idle 1.0)
  ("f" fullscreen-toggle "Fullscreen toggle" :exit t))


(provide 'init-misc)
