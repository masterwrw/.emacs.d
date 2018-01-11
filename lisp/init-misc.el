(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode)

;; Show line content if it is too long
(global-visual-line-mode)


;; Don't create backup files
(setq make-backup-files nil)


;; Remember the cursor position of files when reopening them
(save-place-mode 1)




(provide 'init-misc)
