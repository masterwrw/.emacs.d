;; F1 列出所有 buffer
(global-set-key (kbd "<f1>") 'list-buffers)

;; F2 查找
(global-set-key (kbd "<f2>") 'swiper)

;; F3 关闭其它 frame
(global-set-key (kbd "<f3>") 'delete-other-windows)

;; F4 关闭当前 buffer
(global-set-key (kbd "<f4>") 'kill-buffer)

;; F6 快速打开文件
(global-set-key (kbd "<f6>") 'find-file)

(provide 'init-keymap)
