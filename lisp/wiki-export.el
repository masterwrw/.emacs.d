(require 'org)

(add-to-list 'load-path "~/.emacs.d/lisp")
(setq eye-packages-dir "~/.emacs.d/packages")
(add-to-list 'load-path (concat eye-packages-dir "/emacs-async"))
(add-to-list 'load-path (concat eye-packages-dir "/helm"))
(add-to-list 'load-path (concat eye-packages-dir "/emacs-htmlize")) ;有代码时
(require 'init-my-orgwiki)

;; (org-wiki-make-menu)
(remove-hook 'c++-mode-hook 'build-command)

(provide 'wiki-export)
