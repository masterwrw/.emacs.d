;;;; History
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

(require-maybe 'saveplace)


(require-maybe 'recentf)
(with-eval-after-load 'recentf
  (setq recentf-max-saved-items 200)
  ;;(add-to-list 'recentf-exclude (expand-file-name package-user-dir))
  (add-to-list 'recentf-exclude ".cache")
  (add-to-list 'recentf-exclude ".cask")
  (add-to-list 'recentf-exclude "bookmarks")
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'"))


;; save minibuffer history
(require-maybe 'savehist)
(with-eval-after-load 'savehist
  (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
	history-length 100
	savehist-additional-variables '(mark-ring
					global-mark-ring
					search-ring
					regexp-search-ring
					extended-command-history)
	savehist-autosave-interval nil ;;不开启自动保存，否则会不断的分配内存
	))


;; for quick startup
(add-hook 'after-init-hook
	  (lambda ()
	    (save-place-mode 1)
	    (recentf-mode 1)
	    (savehist-mode 1)))




(provide 'init-history)
