(use-package magit
  :ensure t
  :config
  (setq magit-push-always-verify nil)
  (setq git-commit-summary-max-length 50))


(use-package fullframe
    :ensure t
    :config
    (fullframe magit-status magit-mode-quit-window nil))


(provide 'init-git)
