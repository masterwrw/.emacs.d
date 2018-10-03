;; History
(require 'saveplace)
(add-hook 'after-init-hook 'save-place-mode)

(require 'recentf)
(add-hook 'find-file-hook
	  (lambda ()
	    (unless recentf-mode
	      (recentf-mode)
	      (recentf-track-opened-file))))

(setq recentf-max-saved-items 200)
;;(add-to-list 'recentf-exclude (expand-file-name package-user-dir))
(add-to-list 'recentf-exclude ".cache")
(add-to-list 'recentf-exclude ".cask")
(add-to-list 'recentf-exclude "bookmarks")
(add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")

;; save minibuffer history
(require 'savehist)
(add-hook 'after-init-hook 'savehist-mode)
(setq enable-recursive-minibuffers t ; Allow commands in minibuffers
      history-length 1000
      savehist-additional-variables '(mark-ring
				      global-mark-ring
				      search-ring
				      regexp-search-ring
				      extended-command-history)
      savehist-autosave-interval 60)



(provide 'init-history)
