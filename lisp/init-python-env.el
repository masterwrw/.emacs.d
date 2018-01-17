;;; Python develop environment config
;; https://realpython.com/blog/python/emacs-the-best-python-editor/#ipythonjupyter-integration

(require-package 'python-mode)
(require 'python-mode)


(require-package 'elpy)
(require 'elpy)


(require-package 'py-autopep8)
(require 'py-autopep8)


(require-package 'ein)
(require 'ein)


(require-package 'flycheck)
;; use flycheck not flymake with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))


;; enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)


;;; interpreter setup
;; https://elpy.readthedocs.io/en/latest/ide.html#interpreter-setup
(if *is-windows*
    (setq python-shell-interpreter "jupyter"
          python-shell-interpreter-args "console --simple-prompt")
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt"))




(provide 'init-python-env)
