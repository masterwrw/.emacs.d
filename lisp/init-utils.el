
(defun eye/insert-date ()
  "Insert a date string"
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

(provide 'init-utils)
