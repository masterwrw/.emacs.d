(require 'counsel-etags)
;; Don't ask before rereading the TAGS files if they have changed
(setq tags-revert-without-query t)
;; Don't warn when TAGS files are large
(setq large-file-warning-threshold nil)
;; Setup auto update now
(add-hook 'prog-mode-hook
	  (lambda ()
	    (add-hook 'after-save-hook
		      'counsel-etags-virtual-update-tags 'append 'local)))

(with-eval-after-load 'counsel-etags
  ;; counsel-etags-ignore-directories does NOT support wildcast
  (add-to-list 'counsel-etags-ignore-directories ".git")
  (add-to-list 'counsel-etags-ignore-directories ".svn")
  ;; counsel-etags-ignore-filenames supports wildcast
  (add-to-list 'counsel-etags-ignore-filenames "TAGS")
  (add-to-list 'counsel-etags-ignore-filenames "*.json")
  (add-to-list 'counsel-etags-ignore-filenames "ui_*.h")
  (add-to-list 'counsel-etags-ignore-filenames "*.ui")
  (add-to-list 'counsel-etags-ignore-filenames "moc_*.cpp")
  (add-to-list 'counsel-etags-ignore-filenames "*.rc")
  (add-to-list 'counsel-etags-ignore-filenames "*.qrc")
  (add-to-list 'counsel-etags-ignore-filenames "*.user"))

;; You can change callback counsel-etags-update-tags-backend to update tags file using your own solution,
;;;(setq counsel-etags-update-tags-backend (lambda () (shell-command "find . -type f -iname \"*.[ch]\" | etags -")))

(require 'backward-forward)
(advice-add 'counsel-etags-find-tag-at-point :before #'backward-forward-push-mark-wrapper)
(backward-forward-mode t)
(global-set-key (kbd "<C-left>") 'backward-forward-previous-location)
(global-set-key (kbd "<C-right>") 'backward-forward-next-location)
(ryo-modal-keys
 ("," backward-forward-previous-location)
 ("." backward-forward-next-location)
 )


(require 'dumb-jump)
(advice-add 'dumb-jump-go :before #'backward-forward-push-mark-wrapper)
(ryo-modal-set-key "gs" 'dumb-jump-go) ;;goto symbol


(provide 'init-navigation)
