;;; Manager external service within emacs

(require-package 'prodigy)

(prodigy-define-service
  :name "Blog service"
  :command "nikola"
  :args '("serve" "--browser")
  :cwd "~/blog"
  :tags '(blog)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)



(provide 'init-service-manager)
