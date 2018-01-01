;; autopair
(require-package 'autopair)
(require 'autopair)
(autopair-global-mode)


;; iedit, Edit multiple regions in the same way simultaneously.
(require-package 'iedit)


;; company, auto complete
(require-package 'company)
(require-package 'company-c-headers)
(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load 'company
  '(progn
     (add-to-list 'company-backends 'company-c-headers)))


;; yasnippet
(require-package 'yasnippet)
(require 'yasnippet)
(yas-global-mode 1)


;; helm-ag
(require-package 'helm-ag)
;; Need install the_silver_searcher, https://github.com/ggreer/the_silver_searcher
(global-set-key (kbd "<f8>") 'helm-do-ag-project-root)



(provide 'init-programming)
