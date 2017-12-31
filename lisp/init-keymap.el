;; F1 列出所有 buffer
(defalias 'list-buffers 'ibuffer) ; make ibuffer default
;(defalias 'list-buffers 'ibuffer-other-window) ; make ibuffer-other-window default
(global-set-key (kbd "<f1>") 'list-buffers)

;; F3 关闭其它 frame
(global-set-key (kbd "<f3>") 'delete-other-windows)

;; F4 关闭当前 buffer
(global-set-key (kbd "<f4>") 'kill-buffer)

;; F6 快速打开文件
(global-set-key (kbd "<f6>") 'find-file)

(provide 'init-keymap)
