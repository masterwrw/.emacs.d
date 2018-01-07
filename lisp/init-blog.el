;;; blog package

;; nikola
; (require-package 'nikola)
; (require 'nikola)
; (setq nikola-output-root-directory "~/blog")
; (setq nikola-verbose t)
; (setq nikola-webserver-auto nil)
; (setq nikola-webserver-host "0.0.0.0")
; (setq nikola-webserver-port "8080")
; (setq nikola-webserver-open-browser-p t)
; (setq nikola-deploy-input t)
; (setq nikola-deploy-input-default "New article")
; (setq nikola-build-before-hook-script (concat nikola-output-root-directory "scripts/pre-build.sh"))
; (setq nikola-build-after-hook-script (concat nikola-output-root-directory "scripts/post-build.sh"))
; (setq nikola-deploy-after-hook-script "nikola iarchiver")


;; org2nikola
(add-to-list 'load-path "~/.emacs.d/site-lisp/org2nikola")
(require 'org2nikola)
(setq org2nikola-org-blog-directory "~/blog")
;; OPTIONAL, set the root directory of nikola
;; "~/.config/nikola/posts" contains the *.meta and *.wp
(setq org2nikola-output-root-directory "~/.config/nikola")



(provide 'init-blog)
