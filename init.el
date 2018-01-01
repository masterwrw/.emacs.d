(setq inhibit-startup-message t)

(require 'package)
(setq package-enable-at-startup nil) ; not activat installed packages
(setq package-archives
      '(("gnu" . "https://elpa.emacs-china.org/gnu/")
	("melpa" . "https://elpa.emacs-china.org/melpa/")
	("melpa-stable" . "https://elpa.emacs-china.org/melpa-stable/")))

(package-initialize) ; activate installed packages

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;; Custom configuration
(require 'init-system-env)
(require 'init-ui)
(require 'init-common)

;; Packages and configuration
(require 'init-try)
(require 'init-which-key)
(require 'init-bind-key)
(require 'init-async)
(require 'init-dired-plus)
(require 'init-findr)
(require 'init-iedit)
(require 'init-magit)
(require 'init-swiper)
(require 'init-cnfonts)
(require 'init-color-theme)
(require 'init-helm-ag)
(require 'init-company)
(require 'init-yasnippet)
(require 'init-smex)
;; Using swiper so ido no longer needed
;(require 'init-ido)
(require 'init-autopair)
(require 'init-avy)
(require 'init-misc)
(require 'init-org-bullets)
;(require 'init-tabbar)
(require 'init-ace-window)
(require-package 'ace-jump-mode)
(require-package 'find-file-in-project)
(require-package 'neotree)
(require-package 'bookmark+)
(require 'init-keymap)
(require 'init-site-lisp) ;; Must come before elpa

(fset 'yes-or-no-p 'y-or-n-p)

(require 'server)
(unless (server-running-p)
  (server-start))

;; Disable auto added stuff, see https://www.reddit.com/r/emacs/comments/4q4ixw/how_to_forbid_emacs_to_touch_configuration_files/
(setq custom-file (concat user-emacs-directory "custom-set-variables.el"))
(load custom-file 'noerror)
