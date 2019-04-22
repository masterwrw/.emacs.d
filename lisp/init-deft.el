(with-eval-after-load 'deft
  (setq deft-recursive t)
  (setq deft-use-filename-as-title t)
  (setq deft-extensions '("txt" "tex" "org"))
  (setq deft-directory "~/org/tecs")
  (setq deft-file-limit 30)
  (setq deft-auto-save-interval 0)
  (setq deft-current-sort-method 'title)
  (setq deft-strip-summary-regexp ".*")
  )

(defun deft-or-close ()
  (interactive)
  (if (eq major-mode 'deft-mode)
      (progn (kill-buffer "*Deft*"))
    (deft)
    ))


(provide 'init-deft)
