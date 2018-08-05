(require 'org)
(setq org-ellipsis " ")
(setq org-src-fontify-natively t) ;; 代码块内语法高亮
(setq org-src-tab-acts-natively t)
(setq org-src-fontify-natively t) ;; code block highlight
(setq org-src-window-setup 'current-window)
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'yas-minor-mode)

;; indent content
(setq org-edit-src-content-indentation 0) ;; 默认不缩进
(setq org-startup-indented t)
(setq org-startup-folded (quote overview))
;; hides blank lines between headings
(setq org-cycle-separator-lines 0)
;; always require new line in header below
;;(setq require-final-newline t)
;; calendar start at monday
(setq calendar-week-start-day 1)

(setq org-support-shift-select 1)

;; org-bullets
;;(when (not (eq system-type 'windows-nt))
;;  (use-package org-bullets
;;    :ensure t
;;    :config
;;    (add-hook 'org-mode-hook (lambda ()
;;                               (org-bullets-mode 1)))))



;; Exported to HTML
(use-package htmlize
  :ensure t)

;; Line wrapping
(add-hook 'org-mode-hook
          '(lambda ()
             (visual-line-mode 1)))

(global-set-key (kbd "C-c '") 'org-edit-src-code)

;;; 快速添加 src block，使用 <el 加 tab 键
;; emacs-lisp
(add-to-list 'org-structure-template-alist
             '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))

;; c++
(add-to-list 'org-structure-template-alist
             '("cpp" "#+BEGIN_SRC C++\n?\n#+END_SRC"))

;; lua
(add-to-list 'org-structure-template-alist
             '("lu" "#+BEGIN_SRC lua\n?\n#+END_SRC"))

;; python
(add-to-list 'org-structure-template-alist
             '("py" "#+BEGIN_SRC python\n?\n#+END_SRC"))


  ;;; Custom util function
;; http://wenshanren.org/?p=327
(defun eye/org-insert-src-block (src-code-type)
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
    (insert (format "#+BEGIN_SRC %s\n" src-code-type)) ; use lower string
					;(newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))

;; Advise set auto-save-default to nil
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote("crypt")))
(setq org-crypt-key nil)
					;(setq org-crypt-tag-matcher "secret") ;; Custom tag for crypt

(when (> emacs-major-version 25)
  (use-package org-brain
    :ensure t
    :init
    (setq org-brain-path "~/notebook/notes/brain")
    :config
    (setq org-id-track-globally t)
    (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
    ;;(push '("b" "Brain" plain (function org-brain-goto-end)
    ;;        "* %i%?" :empty-lines 1)
    ;;      org-capture-templates)
    (setq org-brain-visualize-default-choices 'all)
    (setq org-brain-title-max-length 64)
    ;; If org-brain is slow, set this!, if this value is t, the title can not contain slashes(/)
    (setq org-brain-file-entries-use-title t)
    ))


;;; gtd
(require 'org-agenda)
(require 'org-capture)
(require 'find-lisp)

;; full frame show
(setq org-agenda-window-setup 'only-window)

(setq eye/org-agenda-directory "~/notebook/gtd/")
(setq org-agenda-files (list (concat eye/org-agenda-directory "task.org")))

(setq org-default-notes-file (concat eye/org-agenda-directory "inbox.org"))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAITTING(w)" "SOMEDAY(s)" "|" "DONE(d@/!)" "ABORT(a@/!)")
	(sequence "REPORT(r)" "BUG(b)" "|" "FIXED(f)")
	))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
	      ("WAITING" :foreground "orange" :weight bold)
	      ("SOMEDAY" :foreground "magenta" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("ABORT" :foreground "forest green" :weight bold))))


(require 'org-protocol)

(defun transform-square-brackets-to-round-ones(string-to-transform)
  "Transforms [ into ( and ] into ), other chars left unchanged."
  (concat 
   (mapcar #'(lambda (c) (if (equal c ?[) ?\( (if (equal c ?]) ?\) c))) string-to-transform))
  )


(setq org-capture-templates
      '(
	("k"
	 "收集" entry (file+headline "~/notebook/gtd/inbox.org" "Inbox")
         "* %?\n%i\n"
	 :create t)
	
	("s"
	 "重要紧急任务" entry (file+headline "~/notebook/gtd/task.org" "Tasks")
         "* TODO [#A] %?\n%i\n"
	 :create t)

	("d"
	 "重要不紧急任务" entry (file+headline "~/notebook/gtd/task.org" "Tasks")
         "* TODO [#B] %?\n%i\n"
	 :create t)

	("f"
	 "项目任务重要紧急" entry (file+headline "~/notebook/gtd/task.org" "Projects")
         "* TODO [#A] %?\n%i\n"
	 :create t)

	("g"
	 "项目任务重要不紧急" entry (file+headline "~/notebook/gtd/task.org" "Projects")
         "* TODO [#B] %?\n%i\n"
	 :create t)

	;; org-protocol: https://github.com/sprig/org-capture-extension

	("p" 
	 "收集网页内容（自动调用）" entry (file+headline "~/notebook/gtd/inbox.org" "Inbox")
	 "* [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]] \
		%^G\n:PROPERTIES:\n:Created: %U\n:END:\n\n%i\n%?"
	 :create t)
	
	("L" 
	 "收集网页链接（自动调用）" entry (file+headline "~/notebook/gtd/inbox.org" "Urls")
         "* [[%:link][%:description]]\n%?\n"
	 :create t)
	
	))


;; 目标路径不能使用 concat
(setq eye-org-inbox-path (concat eye/org-agenda-directory "inbox.org"))
(setq eye-org-task-path (concat eye/org-agenda-directory "task.org"))
(setq eye-org-finished-path (concat eye/org-agenda-directory "finished.org"))
(setq eye-org-trash-path (concat eye/org-agenda-directory "trash.org"))
(setq eye-org-someday-path (concat eye/org-agenda-directory "someday.org"))
(setq org-refile-targets
      '(
	(eye-org-inbox-path :level . 1)
        (eye-org-task-path :level . 1)
        (eye-org-finished-path :level . 1)
        (eye-org-trash-path :level . 1)
        (eye-org-someday-path :level . 1)
	))

(setq org-archive-location (concat eye/org-agenda-directory "finished.org::"))



(define-key org-src-mode-map (kbd "C-s") 'org-edit-src-save)
(define-key org-src-mode-map (kbd "C-<tab>") 'eye/indent-region-or-buffer)

(defun eye/inbox ()
  (interactive)
  (find-file org-default-notes-file)
  )

(defun eye/task ()
  (interactive)
  (find-file (concat eye/org-agenda-directory "task.org"))
  )

(use-package org-pomodoro
  :ensure t
  :bind
  (:map org-agenda-mode-map
	(("I" . org-pomodoro)))
  :config
  (setq org-pomodoro-format "%s"))



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



(provide 'init-org)
