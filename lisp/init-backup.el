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




(provide 'init-backup)
