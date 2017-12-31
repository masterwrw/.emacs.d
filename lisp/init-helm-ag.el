(require-package 'helm-ag)

;; Need install the_silver_searcher, https://github.com/ggreer/the_silver_searcher
(global-set-key (kbd "<f8>") 'helm-do-ag-project-root)

(provide 'init-helm-ag)
