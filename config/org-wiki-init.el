(require 'org-wiki)

(setq org-wiki-location-list
      '(
        "~/cloud/notebook/tec"    ;; First wiki (root directory) is the default. 
        "~/notebook/private"
        ))

(setq org-wiki-location (car org-wiki-location-list))

;; (setq org-wiki-default-read-only t) ;; 默认是否只读模式
(setq org-wiki-close-root-switch nil) ;; 切换wiki时是否关闭所有当前wiki页面

(setq org-wiki-backup-location "~/archive/backups") ;; Set backup directory

;; Fix can't search, @see https://stackoverflow.com/questions/7014455/executing-rgrep-non-interactively/7016761#7016761
(eval-after-load "grep"
  '(grep-compute-defaults))

;; (setq org-wiki-server-host "127.0.0.1")
;; (setq org-wiki-server-port "8000")
;; (setq org-wiki-emacs-path "c:/Users/arch/opt/emacs/bin/runemacs.exe") ;; 用于导出html时指定emacs路径

;; 设置默认页面模板
;; (setq org-wiki-template
      ;; (string-trim
;; "
;; #+TITLE: %n
;; #+DESCRIPTION:
;; #+KEYWORDS:
;; #+STARTUP:  content
;; #+DATE: %d
;; 
;; - [[wiki:index][Index]]
;; 
;; - Related: 
;; 
;; * %n
;; "))




(provide 'org-wiki-init)
