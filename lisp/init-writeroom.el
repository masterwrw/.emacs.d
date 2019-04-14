;; writeroom
(require-maybe 'writeroom-mode)
(setq writeroom-width 120)

(defun writeroom-mode-on ()
  (interactive)
  (add-hook 'c++-mode-hook 'writeroom-mode)
  (add-hook 'emacs-lisp-mode-hook 'writeroom-mode)
  (add-hook 'org-mode-hook 'writeroom-mode)
  (add-hook 'css-mode-hook 'writeroom-mode)
  (writeroom-mode))

(defun writeroom-mode-off ()
  (interactive)
  (remove-hook 'c++-mode-hook 'writeroom-mode)
  (remove-hook 'emacs-lisp-mode-hook 'writeroom-mode)
  (remove-hook 'org-mode-hook 'writeroom-mode)
  (remove-hook 'css-mode-hook 'writeroom-mode)
  (writeroom-mode -1))


(provide 'init-writeroom)
