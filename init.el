(setq inhibit-startup-message t)

(fset 'yes-or-no-p 'y-or-n-p)

(setq gc-cons-threshold 100000000)

(require 'package)
(setq package-enable-at-startup nil) ; not activat installed packages

(setq package-archives
      '(("gnu" . "http://elpa.emacs-china.org/gnu/")
	("melpa" . "http://elpa.emacs-china.org/melpa/")
	("melpa-stable" . "http://elpa.emacs-china.org/melpa-stable/")
					;	("marmalade" . "http://marmalade-repo.org/packages/")
	("org" . "https://orgmode.org/elpa/")))

(package-initialize) ; activate installed packages

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;; Custom configuration
(require 'init-system-env)
;(when *is-windows*
;    (require 'init-coding))
(require 'init-common)
(require 'init-gui)

;; Packages and configuration
(require 'init-try)
(require 'init-bind-key)
(require 'init-async)
(require 'init-site-lisp)

(require 'init-interface-plus)
(require 'init-file-manager)
(require 'init-helm)
(require 'init-project-manager)
(require 'init-programming)
(require 'init-color-theme)
(require 'init-font)
(require 'init-navigation)
(require 'init-clisp-env)
(require 'init-cpp-env)
(require 'init-python-env)
(require 'init-nsis-env)
(require 'init-newlisp-env)
(require 'init-racket-env)
(require 'init-orgmode)
(require 'init-misc)
(require 'init-rss-feed)
;(require 'init-keyfreq) ; M-v not work
(require 'init-blog)
(require 'init-service-manager)
;(require 'init-evil)
(require 'init-key-binding)



(require 'server)
(unless (server-running-p)
  (server-start))

;; Disable auto added stuff, see https://www.reddit.com/r/emacs/comments/4q4ixw/how_to_forbid_emacs_to_touch_configuration_files/
(setq custom-file (concat user-emacs-directory "custom-set-variables.el"))
(load custom-file 'noerror)
