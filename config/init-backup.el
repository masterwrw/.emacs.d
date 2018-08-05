;; 备份文件 file~，指定备份目录后，文件名为 !drive_f!dirname!dirname!filename~
(setq backup-by-copying t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)
(setq backup-directory-alist '(("." . "~/cache/backups")))
;; 临时文件 #file#
(setq auto-save-file-name-transforms '((".*" "~/cache/backups" t)))


(provide 'init-backup)
