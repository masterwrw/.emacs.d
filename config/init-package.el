;; Emacs27 warning Unnecessary call to package-initialize in init file)
(if (< emacs-major-version 27)    
    (package-initialize)
  (unless package--initialized (package-initialize)))

(require 'package)
(setq package-enable-at-startup nil) ;; not activat installed packages

(defvar site-gnu '("gnu" . "http://elpa.emacs-china.org/gnu/"))
(defvar site-melpa '("melpa" . "http://elpa.emacs-china.org/melpa/"))
(defvar site-melpa-stable '("melpa-stable" . "http://elpa.emacs-china.org/melpa-stable/"))
(defvar site-marmalade '("marmalade" . "http://marmalade-repo.org/packages/"))
(defvar site-org '("org" . "https://orgmode.org/elpa/"))

(setq package-archives nil)
(add-to-list 'package-archives site-melpa-stable t)
(add-to-list 'package-archives site-melpa t)
(add-to-list 'package-archives site-gnu t)
;;(add-to-list 'package-archives site-marmalade t)
;;(package-initialize) ;; activate installed packages

(unless (and (file-exists-p "~/.emacs.d/elpa/archives/gnu")
             (file-exists-p "~/.emacs.d/elpa/archives/melpa")
             (file-exists-p "~/.emacs.d/elpa/archives/melpa-stable"))
  (package-refresh-contents))

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

;;; Bootstrapping use-package
(unless (package-installed-p 'use-package)
  (require-package 'use-package))

(use-package use-package-chords
  :ensure t
  :config (key-chord-mode 1))


;;; el-get
(use-package el-get
  :ensure t)



(provide 'init-package)
