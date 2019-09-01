(require 'org)

(setq user-lisp-dir (concat (file-name-as-directory user-emacs-directory) "lisp"))
(add-to-list 'load-path user-lisp-dir)
(add-to-list 'load-path (concat user-lisp-dir "/org-wiki"))

(defvar eye-packages-dir (expand-file-name "emacs-packages" "~/src"))
(add-to-list 'load-path eye-packages-dir)
(add-to-list 'load-path (concat eye-packages-dir "/emacs-async"))
(add-to-list 'load-path (concat eye-packages-dir "/helm"))
(add-to-list 'load-path (concat eye-packages-dir "/emacs-htmlize")) ;有代码时
(require 'init-org-wiki)

;; (org-wiki-make-menu)
(remove-hook 'c++-mode-hook 'build-command)

(provide 'wiki-export)
