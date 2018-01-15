(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode)
(diminish 'auto-revert-mode)


;; Show line content if it is too long
(global-visual-line-mode)
(diminish 'visual-line-mode)


;; Don't create backup files
(setq make-backup-files nil)


;; Remember the cursor position of files when reopening them
(save-place-mode 1)


;; Set default directory on startup
(setq default-directory "~/.emacs.d")



(provide 'init-misc)
