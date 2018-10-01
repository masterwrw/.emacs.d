;; Fix load slow, https://github.com/raxod502/radian/issues/180
(when tool-bar-mode
  (tool-bar-mode -1)) ;; 禁用工具栏
(when menu-bar-mode
  (menu-bar-mode -1)) ;; 禁用菜单栏
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

(setq ring-bell-function 'ignore) ;; 禁止出现烦人的响铃

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



;; 使用 emacsclient 需要先启动服务
(use-package server
  :ensure nil
  :hook (after-init . server-mode))


(provide 'init-gui)
