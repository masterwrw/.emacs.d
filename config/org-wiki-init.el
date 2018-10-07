(require 'org-wiki)

;; (setq org-wiki-location-list
      ;; '(
        ;; "~/Documents/wiki"    ;; First wiki (root directory) is the default. 
        ;; "~/Documents/wiki2 "
        ;; "~/Documents/wiki3"
        ;; ))

(setq org-wiki-location "~/notebook/org-wiki")

;; (setq org-wiki-default-read-only t) ;; 默认是否只读模式
(setq org-wiki-close-root-switch t) ;; 切换wiki时是否关闭所有当前wiki页面

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
