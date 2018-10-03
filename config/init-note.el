(defvar my-notes-directory "~/notebook/notes/technology/")

(defun search-notes (str)
  "Notes search"
  (interactive"sSearch: ")
  (counsel-ag str my-notes-directory))

(defun new-note (str)
  "Create a new notes."
  (interactive "sNotes name: ")
  (find-file (concat my-notes-directory str ".org")))


;;; deft
(require 'deft)
(setq deft-directory my-notes-directory)
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

(defun deft-other-dir ()
  (interactive)
  (require 'deft)
  (setq deft-directory (read-directory-name "Deft dir: " deft-directory))
  (deft)
  (deft-refresh))

(defun deft-posts ()
  (interactive)
  (if (file-exists-p "~/projects/python/blog/soeye.github.io/posts")
	  (progn
		(require 'deft)
		(setq deft-directory "~/projects/python/blog/soeye.github.io/posts")
		(deft)
		(deft-refresh))
	(message "No post directory.")))
  

(defun deft-all-notes ()
  (interactive)
  (if (file-exists-p "~/notebook/notes")
	  (progn
		(require 'deft)
		(setq deft-directory "~/notebook/notes")
		(deft)
		(deft-refresh))))



(provide 'init-note)
