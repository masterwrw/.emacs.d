(defvar my-notes-directory "~/notebook/notes/tecnology/")

(defun eye/notes-search(str)
  "Notes search"
  (interactive"sSearch: ")
  (counsel-ag str my-notes-directory))

(defun eye/notes-new (str)
  "Create a new notes."
  (interactive "sNotes name: ")
  (find-file (concat my-notes-directory str ".org")))


;;; deft
(use-package deft
  :ensure t
  :config
  (setq deft-directory "~/projects/python/blog/soeye.github.io/posts")
  (setq deft-extensions '("org"))
  (setq deft-recursive t)
  (setq deft-text-mode 'org-mode)
  (setq deft-incremental-search nil)
  (setq deft-use-filename-as-title t)
  (setq deft-strip-summary-regexp (concat "\\("
                                          "^#\\+OPTIONS:.*"
                                          "\\|^#\\+BEGIN.*"
                                          "\\|^\.+ title: "
                                          "\\|^\.+ slug.*"
                                          "\\|^\.+ date.*"
                                          "\\|^\.+ tags.*"
                                          "\\|^\.+ category.*"
                                          "\\|^\.+ link.*"
                                          "\\|^\.+ desc.*"
                                          "\\|^\.+ type.*"
                                          "\\|^#\\+END.*"
                                          "\\)"))
  )

(defun eye/deft-dir ()
  (interactive)
  (require 'deft)
  (setq deft-directory (read-directory-name "Deft dir: " deft-directory))
  (deft))

(defun eye/deft-posts ()
  (interactive)
  (require 'deft)
  (setq deft-directory "~/projects/python/blog/soeye.github.io/posts")
  (deft))

(defun eye/deft-notes ()
  (interactive)
  (require 'deft)
  (setq deft-directory "~/notebook/notes")
  (deft))


;;; nikola
(use-package nikola
  :ensure t
  :config
  (setq nikola-output-root-directory "d:/projects/python/nikola/eye.github.io/")
  (setq nikola-verbose t)
  (setq nikola-webserver-auto t)
  (setq nikola-webserver-host "127.0.0.1")
  (setq nikola-webserver-port "8080")
  (setq nikola-webserver-open-browser-p t)
  (setq nikola-new-post-extension "org")
  ;;(setq nikola-deploy-input t)
  ;;(setq nikola-deploy-input-default "New article")
  ;;(setq nikola-build-before-hook-script (concat nikola-output-root-directory "scripts/pre-build.sh"))
  ;;(setq nikola-build-after-hook-script (concat nikola-output-root-directory "scripts/post-build.sh"))
  ;;(setq nikola-deploy-after-hook-script "nikola iarchiver")

  (when (eq system-type 'windows-nt)
    (progn
      (defun eye/shell-nikola ()
	(interactive)
	(eye/shell-cmd "shell-nikola"
                       (concat "c:\\green-soft\\emacs-25.3_1-x86_64\\bin;"
                               "C:\\Python\\Python36;C:\\Python\\Python36\\Scripts;"
                               )))))
  )


(use-package prodigy
  :ensure t
  :config
  (when (eq system-type 'windows-nt)
    (prodigy-define-service
      :name "Blog service"
      :command "nikola"
      :args '("serve" "--browser")
      :cwd "d:/projects/python/nikola/eye.github.io"
      :tags '(blog)
      :stop-signal 'sigkill
      :kill-process-buffer-on-stop t)))



(provide 'init-note)
