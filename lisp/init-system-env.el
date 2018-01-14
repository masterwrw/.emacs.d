(setq *is-mac* (eq system-type 'darwin))
(setq *is-windows* (eq system-type 'windows-nt))
(setq *is-cygwin* (eq system-type 'cygwin))
(setq *is-linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)))
(setq *is-unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-linux)))
(setq *is-emacs24* (and (not (featurep 'xemacs)) (or (>= emacs-major-version 24))))
(setq *is-emacs25* (and (not (featurep 'xemacs)) (or (>= emacs-major-version 25))))



(provide 'init-system-env)
