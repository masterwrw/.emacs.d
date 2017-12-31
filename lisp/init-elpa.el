;; emacs24 require calling `package-initialize' explicitly
(require 'package)
(package-initialize)

;; Set packages site
(setq package-archives
      '(("gnu" . "https://elpa.emacs-china.org/gnu/")
	("melpa" . "https://elpa.emacs-china.org/melpa/")
	("melpa-stable" . "https://elpa.emacs-china.org/melpa-stable/")))


;; On-demand installation of packages
(defun require-package (package &optional min-version no-refresh)
  "Ask elpa to install given PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

;;------------------------------------------------------------------------------
;; Fire up package.el and ensure the following packages are installed.
;;------------------------------------------------------------------------------
(require-package 'async)
(require-package 'color-theme)
(require-package 'smex)
(require-package 'swiper)
(require-package 'cnfonts)
;; Need install the_silver_searcher, https://github.com/ggreer/the_silver_searcher
(require-package 'helm-ag)
(require-package 'company)
(require-package 'company-c-headers)
(require-package 'magit)
(require-package 'yasnippet)
(require-package 'dired+)
(require-package 'findr)
(require-package 'iedit)
(require-package 'flx-ido)
(require-package 'ace-window)
(require-package 'ace-jump-mode)
(require-package 'find-file-in-project)
(require-package 'neotree)
(require-package 'autopair)
(require-package 'bind-key)
(require-package 'bookmark+)

(provide 'init-elpa)
