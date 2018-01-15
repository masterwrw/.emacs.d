(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda ()
			     (org-bullets-mode 1))))


;; Index for all org files
(load-library "find-lisp")
(if (file-exists-p "~/notebook/notes")
  (setq org-agenda-files (find-lisp-find-files "~/notebook/notes" "\.org$"))




(provide 'init-orgmode)
