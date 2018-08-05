(tool-bar-mode -1) ;; 禁用工具栏
(menu-bar-mode -1) ;; 禁用菜单栏
(if (display-graphic-p)
    (scroll-bar-mode -1) ;; 禁用滚动条 emacs26 -nw will be error
  )

(setq frame-title-format "%b -- %f") ;; 自定义标题栏
(setq inhibit-startup-message t) ;; 禁用启动后显示的消息 buffer
(setq initial-scratch-message nil) ;; 禁止显示 *scratch* buffer 中默认出现的文本


;; 去掉窗口边缘和分割窗口时分割条的边缘
;; http://emacsredux.com/blog/2015/01/18/customizing-the-fringes/
;; linux maybe need gdb, and use mouse to set breakpoint on fringe, so only hide fringe on windows.    
(when (eq system-type 'windows-nt)
  (set-window-fringes nil 0 0) ;; border side
  (fringe-mode '(0 . 0)) ;; middle of split frame
  )

(setq ring-bell-function 'ignore) ;; 禁止出现烦人的响铃

;; 中文字体
(use-package cnfonts
  :ensure t)


;; 最大化
(defun maximize-frame ()
  "Maximizes the active frame in Windows"
  (interactive)
  ;; Send a `WM_SYSCOMMAND' message to the active frame with the
  ;; `SC_MAXIMIZE' parameter.
  (when (eq system-type 'windows-nt)
    (w32-send-sys-command 61488)))

(defun post-load-stuff ()
  (interactive)
  (maximize-frame)
  (set-cursor-color "#AA0000"))

(add-hook 'window-setup-hook 'post-load-stuff t)
(add-hook 'window-setup-hook 'toggle-frame-maximized t)


(setq default-cursor-type 'bar) ;; 光标形状

;; 鼠标滚轮缩放文本大小
(if (eq system-type 'windows-nt)
    (progn
      (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
      (global-set-key (kbd "<C-wheel-up>") 'text-scale-increase))
  (progn
    (global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
    (global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)))


(put 'suspend-frame 'disabled t) ;; 禁止 Ctrl+z 时挂起 emacs

;; 用 y/n 代替 yes/no 的回答
;; (fset 'yes-or-no-p 'y-or-n-p) 相同的效果
(defalias 'yes-or-no-p 'y-or-n-p)


;; 使用 emacsclient 需要先启动服务
(add-hook 'after-init-hook
	  (lambda ()
	    (server-start)))


(provide 'init-gui)
