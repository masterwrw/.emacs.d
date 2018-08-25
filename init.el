(add-to-list 'load-path (concat user-emacs-directory "config"))
(require 'init-begin)
(require 'init-package)
(require 'init-hydra)
(require 'init-benchmark)
(require 'init-theme)
(require 'init-gui)
(require 'init-encoding)
(require 'init-backup)
(require 'init-history)
(require 'init-dired)
(require 'init-external)
(require 'init-git)
(require 'init-dashboard)
(require 'init-web-search)
(require 'init-flycheck)
(require 'init-projectile)
(require 'init-edit)
(require 'init-auto-sudoedit)
(require 'init-yasnippet)
(require 'init-company-mode)
(require 'init-python)
(require 'init-qt)
(require 'init-cpp)
(require 'init-autoit)
(require 'init-php)
(require 'init-elisp)
(require 'init-sql)
(require 'init-navigation)
(require 'init-modeline)
(require 'init-minor)
(require 'init-note)
(require 'init-org)
(require 'init-shell)
(require 'init-tramp)
(require 'init-media)
(require 'init-document)
(require 'init-dict)
(require 'init-key)
(require 'init-end)


;; (require 'org)
;; 
;; (setq config-file (expand-file-name "config.org" "~/.emacs.d/"))
;; (when (file-readable-p config-file)
;; (org-babel-load-file config-file))
