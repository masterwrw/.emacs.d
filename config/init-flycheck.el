;; 不使用 global-flycheck-mode
(require 'flycheck)
(dolist (hook (list
	       'ruby-mode-hook
	       'python-mode-hook
	       'go-mode-hook
	       'c++-mode-hook
	       ))
  (add-hook hook '(lambda () (flycheck-mode 1))))

;; Use posframe as flycheck UI.
(if (>= emacs-major-version 26)
    (with-eval-after-load 'flycheck
      (require 'flycheck-posframe)
      (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)))



(provide 'init-flycheck)
