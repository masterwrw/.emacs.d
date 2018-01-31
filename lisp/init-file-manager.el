(require-package 'dired+)
(require-package 'neotree)
(require-package 'bookmark+)
(require-package 'treemacs)
(require-package 'findr)


;; direc-x configuration, for use virtual dired.
(load "dired-x")

;(add-hook 'dired-load-hook
;          (lambda ()
;            (load "dired-x")
;            ;; Set dired-x global variables here.  For example:
;            ;; (setq dired-guess-shell-gnutar "gtar")
;            ;; (setq dired-x-hands-off-my-keys nil)
;            ))
(add-hook 'dired-mode-hook
          (lambda ()
            ;; Set dired-x buffer-local variables here.  For example:
            (dired-omit-mode 1)
            ))

(setq auto-mode-alist (cons '("[^/]\\.dired$" . dired-virtual-mode)
                                   auto-mode-alist))



(require-package 'fzf)
(require 'fzf)



(provide 'init-file-manager)
