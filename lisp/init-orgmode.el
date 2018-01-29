(if *is-linux*
  (use-package org-bullets
    :ensure t
    :config
    (add-hook 'org-mode-hook (lambda ()
                               (org-bullets-mode 1)))))


;; http://wenshanren.org/?p=327
(defun my-org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("C++" "emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    ;(newline-and-indent) ; no auto indent space
    (insert (format "#+begin_src %s\n" src-code-type)) ; use lower string
    ;(newline-and-indent)
    (insert "#+end_src\n")
    (previous-line 2)
    (org-edit-src-code)))


(defun my-org-insert-quote ()
  "Insert a quote block in org-mode"
  (interactive)
  (progn
    (insert "#+begin_quote\n\n#+end_quote\n")
    (previous-line 2)))


;; Indent content
(setq org-edit-src-content-indentation 0)

;; Syntax highlight
(setq org-src-fontify-natively t)

(setq org-startup-indented t)
(setq org-startup-folded (quote overview))


;;; org capture configuration
(setq org-directory "~/notebook/notes/gtd")
(setq org-default-notes-file (concat org-directory "/inbox.org"))

(setq org-agenda-files (list (concat org-directory "/inbox.org")
                             (concat org-directory "/finished.org")
                             (concat org-directory "/canceled.org")
                             (concat org-directory "/journal.org")))


(setq org-capture-templates
      '(
        ("i" "Inbox" entry (file "~/notebook/notes/gtd/inbox.org")
         "* %?\n" :prepend t :empty-lines 1)

        ("t" "Create Task" entry (file "~/notebook/notes/gtd/todo.org")
         "* %?\n Created on %T\n" :prepend t)

        ("j" "Journal" entry (file+datetree "~/notebook/notes/gtd/journal.org")
         "* %?\n Input time %U\n  %i\n  %a")

        ("s" "Screencast" entry (file+headline "~/notebook/notes/gtd/inbox.org" "Screencast")
         "* %?\n%i\n")

        ))


(setq org-refile-targets
      '(
        ("~/notebook/notes/gtd/finished.org" :level . 1)
        ("~/notebook/notes/gtd/canceled.org" :level . 1)
        ))

(setq org-archive-location "~/notebook/notes/gtd/finished.org::")





;; Index for all org files
;(load-library "find-lisp")
;(if (file-exists-p "~/notebook/notes")
;  (setq org-agenda-files (find-lisp-find-files "~/notebook/notes" "\.org$")))




(provide 'init-orgmode)
