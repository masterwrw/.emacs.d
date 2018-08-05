(use-package counsel-etags
  :ensure t
  :config
  ;; Don't ask before rereading the TAGS files if they have changed
  (setq tags-revert-without-query t)
  ;; Don't warn when TAGS files are large
  (setq large-file-warning-threshold nil)
  ;; Setup auto update now
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'after-save-hook
                        'counsel-etags-virtual-update-tags 'append 'local)))
  :bind
  ("M-/" . 'counsel-etags-find-tag-at-point))

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

(use-package backward-forward
  :ensure t
  :config
  (advice-add 'counsel-etags-find-tag-at-point :before #'backward-forward-push-mark-wrapper)
  (backward-forward-mode t))

(use-package dumb-jump
  :ensure t
  :bind
  ("M-," . 'dumb-jump-back)
  ("M-." . 'dumb-jump-go)
  :config
  (advice-add 'dumb-jump-go :before #'backward-forward-push-mark-wrapper))


(provide 'init-navigation)
