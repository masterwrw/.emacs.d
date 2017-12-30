;; emacs24 require calling `package-initialize' explicitly
(require 'package)
(package-initialize)

;; Set packages site
(setq package-archives
      '(
	("melpa" . "http://elpa.emacs-china.org/melpa/")
	("melpa-stable" . "http://elpa.emacs-china.org/melpa-stable/")))


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
(require-package 'color-theme)
(require-package 'smex)
(require-package 'swiper)
(require-package 'cnfonts)
(require-package 'helm-ag)
(require-package 'company)
(require-package 'magit)


(provide 'init-elpa)
