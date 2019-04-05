;;;; History
(eye--reset-time)
(require 'saveplace)
(save-place-mode 1)
(eye--print-time "require saveplace")

(eye--reset-time)
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 200)
;;(add-to-list 'recentf-exclude (expand-file-name package-user-dir))
(add-to-list 'recentf-exclude ".cache")
(add-to-list 'recentf-exclude ".cask")
(add-to-list 'recentf-exclude "bookmarks")
(add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
(eye--print-time "require recentf")


(provide 'init-history)
