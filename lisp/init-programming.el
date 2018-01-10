;; autopair
(require-package 'autopair)
(require 'autopair)
(autopair-global-mode)


;; change-inner
(require-package 'change-inner)
(require 'change-inner)


;; smartparens
(require-package 'smartparens)
(require 'smartparens)


;; wgrep-ag, Writable grep buffer and apply the changes to files
(require-package 'wgrep)
(require-package 'wgrep-ag)
(require 'wgrep)


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


;; helm-ag, Need install the_silver_searcher, https://github.com/ggreer/the_silver_searcher
(require-package 'helm-ag)




(provide 'init-programming)
