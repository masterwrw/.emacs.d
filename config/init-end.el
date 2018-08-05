;;; init-end --- 所有配置初始化完成后的设置
;;
;; 禁止自动写入一些内容到 init.el 文件后面
;; https://www.reddit.com/r/emacs/comments/4q4ixw/how_to_forbid_emacs_to_touch_configuration_files/

(setq custom-file (concat user-emacs-directory "custom-set-variables.el"))
(load custom-file 'noerror)


(provide 'init-end)
