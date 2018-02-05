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
;; Set auto save file
;(require 'auto-save)
;(auto-save-enable)
;(setq auto-save-slient t)


;; Remember the cursor position of files when reopening them
(save-place-mode 1)


;; Recent file number
(setq recentf-max-saved-items 100)


;; For quick find file in git repo
(setq vc-handled-backends ())


;; Delete selection enable
(delete-selection-mode 1)

(setq shift-select-mode nil)

(setq column-number-mode t)

(setq scroll-step 3)

(display-time)

;; Set default directory on startup
(setq default-directory "~/.emacs.d")


;; Startup windowing
(setq next-line-add-newlines nil)
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)
(split-window-horizontally)
(other-window 1)
					;(split-window-vertically)
					;(other-window 1)
					;(shell)
					;(other-window 1)



(provide 'init-misc)
