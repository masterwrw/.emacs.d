;; Loading the repository from file when on start up.
(bm-repository-load)
;; Saving bookmarks
(add-hook 'kill-buffer-hook #'bm-buffer-save)

;; Saving the repository to file when on exit.
;; kill-buffer-hook is not called when Emacs is killed, so we
;; must save all bookmarks first.
(add-hook 'kill-emacs-hook #'(lambda nil
			       (bm-buffer-save-all)
			       (bm-repository-save)))

;; The `after-save-hook' is not necessary to use to achieve persistence,
;; but it makes the bookmark data in repository more in sync with the file
;; state.
(add-hook 'after-save-hook #'bm-buffer-save)

;; Restoring bookmarks
(add-hook 'find-file-hooks   #'bm-buffer-restore)
(add-hook 'after-revert-hook #'bm-buffer-restore)

;; The `after-revert-hook' is not necessary to use to achieve persistence,
;; but it makes the bookmark data in repository more in sync with the file
;; state. This hook might cause trouble when using packages
;; that automatically reverts the buffer (like vc after a check-in).
;; This can easily be avoided if the package provides a hook that is
;; called before the buffer is reverted (like `vc-before-checkin-hook').
;; Then new bookmarks can be saved before the buffer is reverted.
;; Make sure bookmarks is saved before check-in (and revert-buffer)
(add-hook 'vc-before-checkin-hook #'bm-buffer-save)

;; 设置样式
(set-face-attribute 'bm-persistent-face nil :foreground "#ff7800" :background "#1142AA")


(provide 'init-bm)
