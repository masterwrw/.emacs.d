;;;; orgmode

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

;; (eh-hack-load-path)
;; (add-to-list 'load-path (expand-file-name "modules/org-9.2.2/lisp" user-emacs-directory))

(require-maybe 'org)
(require-maybe 'helm-org)

;;;; org
(defun eye-setup-orgmode ()
  (setq org-ellipsis " ")
  (setq org-src-fontify-natively nil) ;; 代码块内语法高亮
  (setq org-src-tab-acts-natively t)
  ;; (setq org-src-window-setup 'current-window) ;; 在当前window打开
  ;; (add-hook 'org-mode-hook 'yas-minor-mode)
  ;; indent content
  (setq org-edit-src-content-indentation 0) ;; 默认不缩进
  (setq org-startup-indented nil) ;; 是否自动开启org-indent-mode
  (setq org-startup-folded (quote overview))
  ;; hides blank lines between headings
  (setq org-cycle-separator-lines 0)
  ;; always require new line in header below
  ;;(setq require-final-newline t)
  (setq calendar-week-start-day 1) ;; 日历从周一开始显示
  (setq org-support-shift-select 1) ;; 是否支持shift+方向键选择
  (setq org-hide-emphasis-markers t) ;; 隐藏斜体标记/text/，如果要删除，则确保光标移到斜体文字最后
  (setq org-time-stamp-formats '("<%Y-%m-%d>" . "<%Y-%m-%d %H:%M>")) ;去掉默认的%a，避免插入时间时显示“周几”为\x数字
  ;; 用圆形符号表示列表开头，匹配" - "
  (font-lock-add-keywords 'org-mode
                          '(("^ +\\([-*]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Speed keys, @see https://orgmode.org/manual/Speed-keys.html
  ;; quick navigation when cursor is on a headline (before any of the stars)
  ;; ?:for help, n/p/f/b...
  (setq org-use-speed-commands t)

  (defalias 'org-beginning-of-line 'eye/beginniing-of-line)

  ;; Exported to HTML
  (require-maybe 'htmlize)


  ;; Line wrapping
  (add-hook 'org-mode-hook
            '(lambda ()
               (visual-line-mode 1)))

  (global-set-key (kbd "C-c '") 'org-edit-src-code)

  ;; 快速添加 src block，使用 <el 加 tab 键
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

  ;; 交互式选择插入代码块 @See http://wenshanren.org/?p=327
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
  (with-eval-after-load 'org-crypt
    (require-maybe 'org-crypt)
    (org-crypt-use-before-save-magic)
    (setq org-tags-exclude-from-inheritance (quote("crypt")))
    (setq org-crypt-key nil)
    (setq org-crypt-tag-matcher "sec") ;; Custom tag for crypt
    )

  ;; password generator
  (require-maybe 'password-generator)

  (defun eye/open-password-file ()
    "Open my password manager file"
    (interactive)
    (find-file locale-password-file))

;;;; gtd
  (require-maybe 'org-agenda)
  (require-maybe 'org-capture)
  (require-maybe 'find-lisp)

  ;; for task more times repeat a week.
  ;; @see https://stackoverflow.com/questions/8751287/weekly-repeating-tasks-emacs-org-mode
  (require-maybe 'org-install)
  (add-to-list 'org-modules 'org-habit)

  ;; System locale to use for formatting time values.
  (setq system-time-locale "C")         ; Make sure that the weekdays in the
					; time stamps of your Org mode files and
					; in the agenda appear in English.

  (setq org-enforce-todo-dependencies t)

  ;; full frame show
  (setq org-agenda-window-setup 'only-window)

  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
	  (sequence "WAITING(w)" "|" "CANCELLED(c)" "DEFERRED(f)")
	  (sequence "GOAL(g)")
          ))

  (setf org-todo-keyword-faces
	'(("REPEAT" . (:foreground "OliveDrab" :bold t :weight bold))
	  ("NEXT" . (:foreground "SlateBlue3" :bold t :weight bold))
          ("TODO" . (:foreground "cyan" :bold t :weight bold))
          ("STARTED" . (:foreground "springgreen" :bold t :weight bold))
          ("CANCELLED" . (:foreground "#DC143C" :bold t :weight bold))
          ("WAITING" . (:foreground "yellow" :bold t :weight bold))
          ("DEFERRED" . (:foreground "OrangeRed" :bold t :weight bold))
          ("DONE" . (:foreground "gray50" :background "gray30"))
	  ("GOAL" . (:foreground "springgreen" :bold t :weight bold))
	  ))

  ;; tags
  ;; #+TAGS: { @work(w) @life(l) @thinking(t) @study(s) }
  ;; #+TAGS: { @add(a) @bug(b) @fixed(f) }
  ;; (setq org-tag-alist '(("STUDIO" . ?s) ;; company studio office
  ;;                       ("PROJECT" . ?p) ;; difference task at company
  ;;                       ("HOME" . ?h) ;; home
  ;;                       ("MAIL" . ?m) ;; mail somebody
  ;;                       ("LUNCHTIME" . ?l) ;; breakfast lunchtime dinner onway etc. (rest)
  ;;                       ("TOURISM" . ?t) ;; tourism or not at home/company and any where
  ;;                       ("COMPUTER" . ?c)
  ;;                       ;; ("FIELD" . ?f)
  ;;                       ("READING" . ?r))) ;; reading

  (require-maybe 'org-protocol)

  (defun transform-square-brackets-to-round-ones(string-to-transform)
    "Transforms [ into ( and ] into ), other chars left unchanged."
    (concat 
     (mapcar #'(lambda (c) (if (equal c ?[) ?\( (if (equal c ?]) ?\) c))) string-to-transform))
    )

  ;; (setq org-agenda-block-separator nil)
  ;; column view format, can write "#+COLUMNS: ..." to org file also 
  ;; (setq org-columns-default-format "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent)  %DEADLINE(Deadline)")
  (setq org-columns-default-format "%7Goal(Goal) %12DEADLINE(Deadline) %30SCHEDULED(Scheduled) %8TODO(To Do) %1PRIORITY(P) %150ITEM(Detailes)")

  (setq locale-gtd-inbox (concat locale-gtd-dir "/inbox.org"))
  (setq locale-gtd-todo (concat locale-gtd-dir "/tasks.org"))
  (setq locale-gtd-private (concat locale-gtd-dir "/private.org"))
  (setq locale-gtd-archive (concat locale-gtd-dir "/archive.org"))
  (setq locale-gtd-goal (concat locale-gtd-dir "/goals.org"))
  (setq locale-gtd-goal-tpl (concat locale-gtd-dir "/tpl-goals.txt"))

  (setq org-agenda-files (list locale-gtd-todo locale-gtd-private locale-gtd-archive locale-gtd-goal))
  ;; (append-to-list 'org-agenda-files locale-custom-projects)
  (setq org-default-notes-file locale-gtd-inbox)
  (defun eye/open-inbox-file () (interactive) (find-file locale-gtd-inbox))

  (setq org-refile-targets
	'(
          (locale-gtd-todo :level . 1)
          (locale-gtd-private :level . 1)
          (locale-gtd-archive :level . 1)
          ))

  (setq org-archive-location (concat locale-gtd-archive "::"))

;;; custom agenda command
  ;; method 1
  ;; (setq eye/gtd-someday-view
  ;;       `("U" "someday" todo "SOMEDAY"
  ;;         ((org-agenda-files (list locale-gtd-task)))))
  ;; (add-to-list 'org-agenda-custom-commands `,eye/gtd-someday-view)
  ;; method 2
  (setq org-agenda-custom-commands
	'(("w" . "Work")
	  ("wn" "Next" tags-todo "+CATEGORY=\"work\"")
	  ("ww" "Waiting" tags-todo "+TODO=\"WAITING\"+CATEGORY=\"work\"")

	  ("p" . "Private")
	  ("pn" "Next" tags-todo "+CATEGORY=\"private\"")
	  ("pw" "Waiting" tags-todo "+TODO=\"WAITING\"+CATEGORY=\"private\"")

	  ;; can use org-agenda T also
	  ("n" "All Next" todo "NEXT")
	  ("v" "All Waiting" todo "WAITING")
	  ("D" "Agenda(week) + Next" ((agenda) (todo "NEXT")))
	  
	  ;; ("z" "Waiting" todo "WAITING")
	  ("d" "Day" agenda "" ((org-agenda-span 1) ;limits display to a single day
				(org-agenda-sorting-strategy
				 (quote ((agenda time-up priority-down tag-up) )))
				(org-deadline-warning-days 0)
				(org-agenda-remove-tags t) ;don't show tags
				))
	  ("f" "Test" ((tags "Goal=\"Long\""
			     ((org-agenda-overriding-header "Long term goals")))
		       (tags "Goal=\"Medium\""
			     ((org-agenda-overriding-header "Medium term goals")))
		       (tags "Goal=\"Short\""
			     ((org-agenda-overriding-header "Short term goals")))
		       ))
	  
	  ("g" "Weekly Goals Review"
	   ((tags "Goal=\"Long\""
		  ((org-agenda-overriding-header "Long term goals")))
	    (tags "Goal=\"Medium\""
		  ((org-agenda-overriding-header "Medium term goals")))
	    (tags "Goal=\"Short\""
		  ((org-agenda-overriding-header "Short term goals")))
	    (tags-todo "Goal=\"\""
		       ((org-agenda-overriding-header "Actions that don't contribute to a goal yet"))))
	   )

	  ("G" "Goals" tags "GOAL-TODO=\"GOAL\"" nil)
	  
	  ))

  (defun eye/open-agenda-day-view ()
    (interactive)
    (org-agenda nil "d"))

  ;; (add-hook 'after-init-hook #'eye/open-agenda-day-view)

  ;; 模板中的file路径不是绝对路径时，将会使用org-directory进行查找
  (setq org-directory locale-gtd-dir)

  ;; capture 的目标路径不能直接使用 concat
  (setq org-capture-templates
	'(
          ("i"
           "inbox" entry (file+headline "inbox.org" "Inbox")
           "* %?\n%i\n"
           :create t)

	  ;; Create Todo under GTD.org -> Work -> Tasks
	  ;; file+olp specifies to full path to fill the Template
	  ("w" "Work TODO" entry (file+olp "tasks.org" "Work" "Tasks")
	   "* TODO %? \n:PROPERTIES:\n:CREATED: %U\n:END:")
	  ;; Create Todo under GTD.org -> Private -> Tasks
	  ;; file+olp specifies to full path to fill the Template
	  ("m" "Private TODO" entry (file+olp "private.org" "Private" "Tasks")
           "* TODO %? \n:PROPERTIES:\n:CREATED: %U\n:END:")

	  ("g" "Goals") 
	  ("ge" "Epic goals" entry (file+headline "goals.org" 
						  "Epic goals") (file "tpl-goals.txt") :empty-lines-after 1) 
	  ("gl" "Long term goal (2-5 years from now)" entry (file+headline "goals.org"
									   "Long term goals (2-5 years from now)") (file "tpl-goals.txt") :empty-lines-after 1) 
	  ("gm" "Medium term goal (6 months up to 2 years)" entry (file+headline "goals.org"
										 "Medium term goalsl (6 months up to 2 years)") (file "tpl-goals.txt") :empty-lines-after 1) 
	  ("gs" "Short term goals (next 6 months)" entry (file+headline "goals.org" 
									"Short term goals (next 6 months)") (file "tpl-goals.txt") :empty-lines-after 1)

          ;; org-protocol: https://github.com/sprig/org-capture-extension

          ("p" 
           "收集网页内容（自动调用）" entry (file+headline "inbox.org" "INBOX")
           "* [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]] \
                %^G\n:PROPERTIES:\n:Created: %U\n:END:\n\n%i\n%?"
           :create t)
          
          ("L" 
           "收集网页链接（自动调用）" entry (file+headline "inbox.org" "LINKS")
           "* [[%:link][%:description]]\n%?\n"
           :create t)
          
          ))


;;;; Notebook
  (require-maybe 'org-attach)
  (add-to-list 'org-modules 'org-attach)
  (setq org-attach-directory locale-notebook-attachment-dir)

  (defun eye/notes-search-keyword ()
    (interactive)
    (let ((keyword (read-string "Search note keyword: " (eye/current-word))))
      (counsel-rg keyword locale-notebook-dir)
      ))

  (defun eye/notes-search-file ()
    (interactive)
    (let ((keyword (read-string "Search note file: ")))
      (dired locale-notebook-dir)
      (if (fboundp 'swiper)
	  (swiper keyword)
	(isearch-forward keyword))
      ))

  (defun eye/notes-dired ()
    (interactive)
    (dired locale-notebook-dir))

  (defun eye/notes-new ()
    (interactive)
    (let ((name (read-string "New note(no suffix): ")))
      (find-file (concat locale-notebook-dir "/" name ".org"))
      (set-buffer-file-coding-system 'utf-8-with-signature-unix 't) ;; 设置编码
      (insert (concat "* " name)) ;; 添加一级标题
      (org-attach-set-inherit)	; use same attach directoryemphatically
      ))

  (defun eye/notes-create-attachment ()
    "创建文件对应的附件文件夹"
    (interactive)
    (let* ((name (replace-regexp-in-string ".org" "" (buffer-name)))
	   (dir (concat locale-notebook-attachment-dir "/" name)))
      (unless (f-directory? locale-notebook-attachment-dir) (f-mkdir locale-notebook-attachment-dir)) ;; 创建附件主目录
      (unless (f-directory? dir) ;; 创建附件子目录
	(f-mkdir dir))
      ))

  (defun eye/notes-open-attachment ()
    "打开与当前文件名相同的附件文件夹"
    (interactive)
    (let* ((name (replace-regexp-in-string ".org" "" (buffer-name)))
	   (dir (concat locale-notebook-attachment-dir "/" name)))
      (if (f-directory? dir)
	  (progn
	    (shell-command (concat "explorer "
				   (encode-coding-string
				    (replace-regexp-in-string "/" "\\\\" dir) 'gbk-dos))) ;; 转换为windows路径后再转换为gbk编码，否则无法打开中文目录
	    )
	(message "Attachment folder not exists!")	
	)))


  (defun eye/org-meta-return ()
    (interactive)
    (org-end-of-line)
    (org-meta-return))
  (define-key org-mode-map (kbd "M-RET") 'eye/org-meta-return)
  )



(defhydra+ hydra-funcs (:idle 1.0)
  ("c" org-capture "Capture" :exit t)
  ("a" org-agenda "Agenda" :exit t))


(defhydra hydra-note (:exit t :idle 1.0)
  ("d" eye/notes-dired "Notes dir")
  ("n" eye/notes-new "New note")
  ("a" eye/notes-create-attachment "Create attach dir")
  ("o" eye/notes-open-attachment "Open attach")
  ("s" eye/notes-search-keyword "Search word")
  ("f" eye/notes-search-file "Search file"))
(eye-define-leader-key global-map "n" 'hydra-note/body)



(with-eval-after-load 'org
  (eye-setup-orgmode)
  (eye-set-leader-key org-mode-map)
  (with-eval-after-load 'org-agenda
    (eye-set-leader-key org-agenda-mode-map))
  )




(provide 'init-orgmode)
