;;; helm configuration

(require-package 'helm)
(require 'helm-config)
(require 'helm-grep)

(helm-mode 1)

(require-package 'helm-swoop)
(require-package 'helm-projectile)

(if (version< "26.0.50" emacs-version)
    (eval-when-compile (require 'helm-lib)))

(helm-projectile-on)
;(projectile-completion-system 'helm)
(setq projectile-indexing-method 'alien)


;; helm-gtags
(require-package 'helm-gtags)
(setq helm-gtags-ignore-case t)
(setq helm-gtags-auto-update t)
(setq helm-gtags-pulse-at-cursor t)

;; Enable helm-gtags-mode in Dired so you can jump to any tag
;; when navigate project tree with Dired
(add-hook 'dired-mode-hook 'helm-gtags-mode)

;; Enable helm-gtags-mode in Eshell for the same reason as above
(add-hook 'eshell-mode-hook 'helm-gtags-mode)

;; Enable helm-gtags-mode in languages that GNU Global supports
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)




(provide 'init-helm)
