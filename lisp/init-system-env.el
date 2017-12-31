(setq *is-a-mac* (eq system-type 'darwin))
(setq *win64* (eq system-type 'windows-nt))
(setq *cygwin* (eq system-type 'cygwin))
(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)))
(setq *unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-linux)))
(setq *emacs24* (and (not (featurep 'xemacs)) (or (>= emacs-major-version 24))))
(setq *emacs25* (and (not (featurep 'xemacs)) (or (>= emacs-major-version 25))))
(setq *no-memory* (cond
                   (*is-a-mac*
                    (< (string-to-number (nth 1 (split-string (shell-command-to-string "sysctl hw.physmem")))) 4000000000))
                   (*linux* nil)
                   (t nil)))

(setq *emacs24old*  (or (and (= emacs-major-version 24) (= emacs-minor-version 3))
                        (not *emacs24*)))

(provide 'init-system-env)
