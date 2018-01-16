(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode)
(diminish 'auto-revert-mode)


;; Show line content if it is too long
(global-visual-line-mode)
(diminish 'visual-line-mode)


;; Don't create backup files
(setq make-backup-files nil)
(setq backup-inhibited t)


;; Stop auto save file to #autosave#
(setq auto-save-default nil)


;; Remember the cursor position of files when reopening them
(save-place-mode 1)


;; Recent file number
(setq recentf-max-saved-items 100)


;; Set default directory on startup
(setq default-directory "~/.emacs.d")



(provide 'init-misc)
