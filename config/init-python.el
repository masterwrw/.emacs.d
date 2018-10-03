(defun install-python-env ()
  (require 'jinja2-mode)
  ;;    (add-to-list 'auto-mode-alist '("\\.tmpl$" . jinja2-mode)))
  )

(defun eye/shell-python3 ()
  (interactive)
  (eye/shell-cmd "shell-python3" "C:\\Python\\Python36;C:\\Python\\Python36\\Scripts;")
  )

(defun eye/python-help ()
  "Find python online document."
  (interactive)
  (let ((url "https://docs.python.org/3.5/search.html?q="))
    (setq url (concat url (read-string "Query python document: " (eye/current-word))))
    (browse-url-firefox url)))


(add-hook 'python-mode-hook 'yas-minor-mode)
 

(provide 'init-python)
