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

(eh-hack-load-path)

;; Require latest org
(require-package 'org)

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

(require 'org)


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
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)" "CANCELLED(c)"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold))))

;;;==========================Calendar========================================
(setq calendar-week-start-day 1) ;; Start at monday



;;; ========================org capture configuration========================
(setq org-directory "~/notebook/notes/gtd")
(setq org-default-notes-file (concat org-directory "/inbox.org"))

(setq org-agenda-files (list (concat org-directory "/inbox.org")
                             (concat org-directory "/finished.org")
                             (concat org-directory "/canceled.org")))

(defun my-inbox ()
  (interactive)
  (find-file org-default-notes-file))

(setq org-capture-templates
      '(

        ("t" "Add to task" entry (file+headline "~/notebook/notes/gtd/inbox.org" "Tasks")
         "* %?\n Created on %T\n" :prepend t :jump-to-captured t)

        ("p" "Add to project" entry (file+headline "~/notebook/notes/gtd/inbox.org" "Project")
         "* %?\n%i\n" :jump-to-captured t)

	("b" "Add to book" entry (file+headline "~/notebook/notes/gtd/inbox.org" "Book")
         "* %?\n%i\n")

	("c" "Add to calendar" entry (file+headline "~/notebook/notes/gtd/inbox.org" "Calendar")
         "* %?\n%i\n" :jump-to-captured t)

	("i" "Add to inbox" entry (file+headline "~/notebook/notes/gtd/inbox.org" "Inbox")
         "* %?\n%i" :prepend t :empty-lines 1)
        ))


(setq org-refile-targets
      '(
        ("~/notebook/notes/gtd/finished.org" :level . 1)
        ("~/notebook/notes/gtd/canceled.org" :level . 1)
        ))

(setq org-archive-location "~/notebook/notes/gtd/finished.org::")




;;; org-brain, require org-mode version 9
(require-package 'org-brain)
(require 'org-brain)
(setq org-brain-path "~/notebook/notes/brain")
;(eval-after-load 'evil
;  (evil-set-initial-state 'org-brain-visualize-mode 'emacs))

(setq org-id-track-globally t)
(setq org-id-locations-file "~/.emacs.d/.org-id-locations")
(setq org-brain-visualize-default-choices 'all)
(setq org-brain-title-max-length 64)
;; If org-brain is slow, set this!, if this value is t, the title can not contain slashes(/)
(setq org-brain-file-entries-use-title t)

(require-package 'uuidgen)
(require 'uuidgen)


;;; Not use capture, because will be add child but no new file
;; Need insert uuid
;(defun my-auto-insert-uuid ()
;    (replace-string "(uuid)" (uuidgen-4)))

;(push '("b" "Brain" plain (function org-brain-goto-end)
;	"* %i%?\n:PROPERTIES:\n:ID:       (uuid)\n:END:" :empty-lines 1)
;      org-capture-templates)
;(add-hook 'org-capture-prepare-finalize-hook 'my-auto-insert-uuid) //works

;(push '("b" "Add to brain" plain (function org-brain-goto-end)
;	"* %i%?" :empty-lines 1)
;      org-capture-templates)
;(add-hook 'org-capture-prepare-finalize-hook 'org-id-get-create)


(require-package 'org-cliplink)
(require 'org-cliplink)

(defun org-brain-cliplink-resource ()
  "Add a URL from the clipboard as an org-brain resource.
Suggest the URL title as a description for resource."
  (interactive)
  (let ((url (org-cliplink-clipboard-content)))
    (org-brain-add-resource
     url
     (org-cliplink-retrieve-title-synchronously url)
     t)))

(define-key org-brain-visualize-mode-map (kbd "L") #'org-brain-cliplink-resource)


(defhydra hydra-org-brain (:color blue)
  "
_c_ Add child(advise)  _p_ New parent  _f_ Add friendship
_h_ New child  _C_ Remove child  _P_ Remove parent  _F_ Remove friendship
_d_ Delete entry  _l_ Add resource  _L_ Add link resource
_n_ Pin  _t_ Set title  _T_ Set tag  _r_ Random  _R_ Random circle
_j_ Goto next link _b_ Go back _v_ Open brain  _C-r_ Rename file
_k_ Goto previous link  _q_ quit
"
  ("j" forward-button)
  ("k" backward-button)
  ("b" org-brain-visualize-back)
  ("h" org-brain-new-child)
  ("*" org-brain-new-child)
  ("c" org-brain-add-child)
  ("C" org-brain-remove-child)
  ("p" org-brain-add-parent)
  ("P" org-brain-remove-parent)
  ("f" org-brain-add-friendship)
  ("F" org-brain-remove-friendship)
  ("n" org-brain-pin)
  ("t" org-brain-set-title)
  ("T" org-brain-set-tags)
  ("d" org-brain-delete-entry)
  ("l" org-brain-visualize-add-resource)
  ("L" org-brain-cliplink-resource)
  ("C-y" org-brain-visualize-paste-resource)
  ("a" org-brain-visualize-attach)
  ("o" org-brain-goto-current)
  ("O" org-brain-goto)
  ("v" org-brain-visualize)
  ("r" org-brain-visualize-random)
  ("R" org-brain-visualize-wander)
  ("C-r" org-brain-rename-file)
  ("q" nil))




(provide 'init-orgmode)
