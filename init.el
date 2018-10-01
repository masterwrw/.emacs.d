
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path (concat user-emacs-directory "config"))
(require 'init-begin)
(require 'init-package)
(require 'init-benchmark)

(use-package benchmark-init
  :init
  (benchmark-init/activate)
  :hook
  (after-init . benchmark-init/deactivate))

(require 'init-encoding)
(require 'init-backup)
(require 'init-history)
(require 'init-shell)
(require 'base-toolkit)
(require 'init-ryo)
(require 'init-ivy)

(require 'idle-require)
(setq idle-require-idle-delay 2)
(setq idle-require-symbols '(init-theme
			     init-gui
			     init-modeline
			     init-dired
			     init-external
			     init-minor
			     init-git
			     init-web-search
			     init-org
			     init-company-mode
			     init-python
			     init-qt
			     init-cpp
			     init-elisp
			     init-php
			     init-sql
			     init-navigation
			     init-note
			     init-yasnippet
			     init-tramp
			     init-media
			     init-document
			     init-dict
			     ))

(if (>= emacs-major-version 26)
    (add-to-list 'idle-require-symbols 'init-flycheck))

(idle-require-mode 1)


(require 'init-edit)

(require 'init-end)
