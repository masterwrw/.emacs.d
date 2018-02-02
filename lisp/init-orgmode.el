;;; Org mode configuration

;; Disable buildin org, https://emacs-china.org/t/topic/3931/3
(defun eh-hack-load-path ()
  ;; Delete buildin org's PATH
  (setq load-path
        (cl-remove-if
         #'(lambda (path)
             (string-match "lisp/org$" path))
         load-path))
  ;; Demove property lists to defeat cus-load and remove autoloads
  (mapatoms
   #'(lambda (sym)
       (let ((sym-name (symbol-name sym)))
         (when (string-match "^\\(org\\|ob\\|ox\\)-?" sym-name)
           (setplist sym nil)
           (when (autoloadp sym)
             (unintern sym)))))))

;; emacs 27 used orgmode 9
;(if (< emacs-major-version 27)
;    (progn ;; Require latest org
;      (eh-hack-load-path)
;      (require-package 'org)
;      (require 'org)
;      (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))))



;; Require org-bullets
(if *is-linux*
  (use-package org-bullets
    :ensure t
    :config
    (add-hook 'org-mode-hook (lambda ()
                               (org-bullets-mode 1)))))


;;; Custom util function
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


;;; ========================Org configuration==============================
;; Indent content
(setq org-edit-src-content-indentation 0)

;; Syntax highlight
(setq org-src-fontify-natively t)

(setq org-startup-indented t)
(setq org-startup-folded (quote overview))

;; Hides blank lines between headings
(setq org-cycle-separator-lines 0)

(setq require-final-newline t)

(defhydra org-mode-hydra (:colur blue)
  "
_a_ Agenda
_s_ Store link   _d_ Toggle link display  _c_ Schedule
_l_ Insert link
_w_ Switch
_q_ quit
"
  ("a" org-agenda)
  ("s" org-store-link)
  ("d" org-toggle-link-display)
  ("c" org-schedule)
  ("l" org-insert-link)
  ("w" org-iswitchb)
  ("q" nil))


(setq org-todo-keywords
      '((sequence "TODO(t!)" "NEXT(n)" "WAITTING(w)" "SOMEDAY(s)" "|" "DONE(d@/!)" "ABORT(a@/!)")
	))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
	      ("WAITING" :foreground "orange" :weight bold)
	      ("SOMEDAY" :foreground "magenta" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("ABORT" :foreground "forest green" :weight bold))))

;;;==========================Calendar========================================
(setq calendar-week-start-day 1) ;; Start at monday



;;; ========================org capture configuration========================
(setq org-directory "~/notebook/notes/gtd")
(setq org-default-notes-file (concat org-directory "/inbox.org"))

(setq org-agenda-files (list (concat org-directory "/inbox.org")
			     (concat org-directory "/task.org")
			     (concat org-directory "/note.org")
                             (concat org-directory "/project.org")))

(defun my-inbox ()
  (interactive)
  (find-file org-default-notes-file))


(setq org-capture-templates
      '(

        ("n" "New" entry (file "~/notebook/notes/gtd/inbox.org")
         "* %?\n%i\n")

	("t" "Task" entry (file+headline "~/notebook/notes/gtd/task.org" "Tasks")
         "* %?\n%i\n")

	("i" "Ideas" entry (file+headline "~/notebook/notes/gtd/task.org" "Ideas")
         "* %?\n%i\n" :jump-to-captured t)

	("c" "Calendar" entry (file+headline "~/notebook/notes/gtd/task.org" "Calendar")
         "* %?\nSCHEDULED: %^t\n%i\n" :jump-to-captured t)

	("r" "Note" entry (file "~/notebook/notes/gtd/note.org")
         "* %?\n%i" :prepend t :empty-lines 1)

        ("p" "Project" entry (file+headline "~/notebook/notes/gtd/inbox.org" "Project")
         "* %?\n%i\n" :jump-to-captured t)

        ))


(setq org-refile-targets
      '(
        ("~/notebook/notes/gtd/finished.org" :level . 1)
        ("~/notebook/notes/gtd/trash.org" :level . 1)
        ))

(setq org-archive-location "~/notebook/notes/gtd/finished.org::")


(if (> emacs-major-version 25)
    (require 'org-brain))


;;;=====================org crypt======================================
;; Advise set auto-save-default to nil
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote("crypt")))
(setq org-crypt-key nil)
;(setq org-crypt-tag-matcher "secret") ;; Custom tag for crypt



(provide 'init-orgmode)
