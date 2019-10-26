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

;; (defvar today-journal-path nil "Shold be ~/org/private/journal/y-m-d.org")
(setq eye-journal-dir (concat locale-notebook-dir "/journal"))

;;;; org
(defun eye-setup-orgmode ()
  (setq org-ellipsis " ")
  (setq org-src-fontify-natively t) ;; 代码块内语法高亮
  (setq org-src-tab-acts-natively t)
  ;; (setq org-src-window-setup 'current-window) ;; 在当前window打开
  ;; (add-hook 'org-mode-hook 'yas-minor-mode)
  ;; indent content
  (setq org-edit-src-content-indentation 0) ;; 代码块默认不缩进
  (setq org-startup-indented t) ;; 是否自动开启org-indent-mode
  (setq org-startup-folded (quote overview))
  ;; hides blank lines between headings
  (setq org-cycle-separator-lines 0)
  ;; always require new line in header below
  (setq require-final-newline t)
  (setq org-tags-column 0)		;; 在org文件中，使tags跟在标题后面
  (setq org-return-follows-link nil) ;; 是否回车打开link
  (setq calendar-week-start-day 1) ;; 日历从周一开始显示
  (setq org-support-shift-select 1) ;; 是否支持shift+方向键选择
  (setq org-hide-emphasis-markers t) ;; 隐藏斜体标记/text/，如果要删除，则确保光标移到斜体文字最后
  ;; 用圆形符号表示列表开头，匹配" - "
  (font-lock-add-keywords 'org-mode
                          '(("^ +\\([-*]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Speed keys, @see https://orgmode.org/manual/Speed-keys.html
  ;; quick navigation when cursor is on a headline (before any of the stars)
  ;; ?:for help, n/p/f/b...
  (setq org-use-speed-commands t)

  (defalias 'org-beginning-of-line 'eye/beginniing-of-line)

  ;; (defun eye/org-meta-return ()
  ;;   "确保按下M-RET时不会打断当前行（但是折叠有属性或内容时会打断属性）"
  ;;   (interactive)
  ;;   (org-end-of-line)
  ;;   (org-meta-return))
  (define-key org-mode-map (kbd "M-RET") 'org-insert-heading-respect-content)

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

  ;; support babel execute
  ;; (org-babel-do-load-languages
   ;; 'org-babel-load-languages '((emacs-lisp . t)))

  ;; password generator
  (require-maybe 'password-generator)

  (defun eye/open-password-file ()
    "Open my password manager file"
    (interactive)
    (find-file (expand-file-name "private/password.org" locale-notebook-dir)))

;;;; gtd
  (require 'org-agenda)
  (require-maybe 'org-capture)
  (require-maybe 'find-lisp)

  ;; for task more times repeat a week.
  ;; @see https://stackoverflow.com/questions/8751287/weekly-repeating-tasks-emacs-org-mode
  (require-maybe 'org-install)
  (add-to-list 'org-modules 'org-habit)

  ;; System locale to use for formatting time values.
  ;; current is "zh_CN.UTF-8", if set to "C", Make sure that the weekdays in the
  ;; time stamps of your Org mode files and in the agenda appear in English.
  (setq system-time-locale "C")
  ;;%a表示插入时间时显示“周几”，如果没有设置system-time-locale为"C"的话，会显示乱码
  (setq org-time-stamp-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M>"))

  (setq org-enforce-todo-dependencies t)

  ;; full frame show
  (setq org-agenda-window-setup 'only-window)

  (setq org-agenda-align-tags-to-column 1) ;; 在agenda中，使tags跟在标题后面


  (setq org-todo-keywords
	'((sequence "INBOX(i)")
	  (sequence "TASK(t)" "SOMEDAY(s)" "REPEAT(r)" "CALENDAR(c)")
	  (sequence "TODO(T)" "ACTION(a)" "WAIT(w)" "DONE(d!)" "|" "CANCELLED(C)" "DEFERRED(f)")
	  (sequence "PLAN(p)")
          ))

  (setf org-todo-keyword-faces
	'(("INBOX" . (:foreground "OliveDrab" :bold t :weight bold))
	  ("TASK" . (:foreground "OrangeRed" :bold t :weight bold))
	  ("SOMEDAY" . (:foreground "yellow" :bold t :weight bold))
	  ("REPEAT" . (:foreground "#009900" :bold t :weight bold))
	  ("TODO" . (:foreground "#009900" :bold t :weight bold))
          ("ACTION" . (:foreground "cyan" :bold t :weight bold))
	  ("WAIT" . (:foreground "yellow" :bold t :weight bold))
	  ("DONE" . (:foreground "#009900" :bold t :weight bold))
          ("CANCELLED" . (:foreground "gray50" :bold t :weight bold))
          ("DONE" . (:foreground "gray50" :background "gray30"))
	  ))

  ;; tags
  ;; #+TAGS: { @work(w) @life(l) @thinking(t) @study(s) }
  ;; #+TAGS: { @add(a) @bug(b) @fixed(f) }
  ;; (setq org-tag-alist '(("work" . ?w)
  ;; 			("learning" . ?l)
  ;; 			("english" . ?e)
  ;; 			("train" . ?t)
  ;; 			("other" . ?o)))

  ;; @see https://github.com/batsibe/org-journal
  (require 'org-journal)
  (setq org-journal-file-type 'daily)
  (setq org-journal-dir "h:/wikinote/journal/")
  (setq org-journal-file-format "%Y-%m-%d.org")
  ;; (setq today-journal-path (org-journal-find-location))
  (defun org-journal-date-format-func (time)
  "Custom function to insert journal date header.
  When buffer is empty prepend a header in front the entry header."
  (concat (when (= (buffer-size) 0)
            (concat
             (pcase org-journal-file-type
               (`daily "#+TITLE: Daily Journal")
               (`weekly "#+TITLE: Weekly Journal")
               (`monthly "#+TITLE: Monthly Journal")
               (`yearly "#+TITLE: Yearly Journal"))))
          org-journal-date-prefix
          (format-time-string "%Y-%m-%d" time)))

  ;; (setq org-journal-date-format 'org-journal-date-format-func)
  (setq org-journal-date-format "%A, %x")

  (require 'org-protocol)

  (defun transform-square-brackets-to-round-ones(string-to-transform)
    "Transforms [ into ( and ] into ), other chars left unchanged."
    (concat 
     (mapcar #'(lambda (c) (if (equal c ?[) ?\( (if (equal c ?]) ?\) c))) string-to-transform))
    )

  ;; (setq org-agenda-block-separator nil)
  ;; column view format, can write "#+COLUMNS: ..." to org file also 
  ;; (setq org-columns-default-format "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent)  %DEADLINE(Deadline)")
  (setq org-columns-default-format "%7Goal(Goal) %12DEADLINE(Deadline) %30SCHEDULED(Scheduled) %8TODO(To Do) %1PRIORITY(P) %150ITEM(Detailes)")
  (setq org-agenda-align-tags-to-column 1) ;在agenda视图中，使tags向左边对齐，默认是auto，向右边对齐，会换行显示

  ;; (setq org-agenda-files (list
			  ;; (expand-file-name "work/todo.org" locale-notebook-dir)
			  ;; (expand-file-name "private/todo.org" locale-notebook-dir)
			  ;; (expand-file-name "private/goals.org" locale-notebook-dir)
			  ;; (expand-file-name "private/journal.org" locale-notebook-dir)
  ;; ))
  ;; 设置所有以Todo开头的org文件
  ;; (setq org-agenda-files (directory-files locale-notebook-dir t "Todo.*.org$"))
  (setq org-agenda-files '("h:/wikinote/ats.org"))
  ;; 添加日志文件
  ;; (dolist (file (directory-files locale-notebook-dir t "Journal.*.org$"))
    ;; (add-to-list 'org-agenda-files file))
  ;; (append-to-list 'org-agenda-files locale-custom-projects)
  (setq org-default-notes-file (expand-file-name "Inbox.org" locale-notebook-dir))
  (defun eye/open-inbox-file () (interactive) (find-file org-default-notes-file))

  (setq org-refile-targets
  	`(
          (,(expand-file-name "Trash.org" locale-notebook-dir) :level . 1)
	  (,(expand-file-name "Note.org" locale-notebook-dir) :level . 1)
          (,(expand-file-name "ats.org" locale-notebook-dir) :level . 1)
          ))

  ;; org-archive-subtree moving an tree to archive file
  ;; settings on org file #+ARCHIVE file head or ARCHIVE PROPERTY
  ;;(setq org-archive-location (concat (expand-file-name "gtd/archive.org" locale-notebook-dir) "::"))

;;; custom agenda command
  ;; method 1
  ;; (setq eye/gtd-someday-view
  ;;       `("U" "someday" todo "SOMEDAY"
  ;;         ((org-agenda-files (list locale-gtd-task)))))
  ;; (add-to-list 'org-agenda-custom-commands `,eye/gtd-someday-view)
  ;; method 2
  (setq org-agenda-custom-commands
	'(
	  ("i" "View inbox" todo "INBOX")
	  ("t" "View tasks" todo "TASK|SOMEDAY|REPEAT|CALENDAR")
	  ("o" "View todo" todo "TODO")
	  ("x" "View action" todo "ACTION")

	  ("z" "View tasks"
	   ((todo "TASK"
	  	  ((org-agenda-overriding-header "任务")))
	    (todo "SOMEDAY"
	  	  ((org-agenda-overriding-header "将来/也许")))
	    (todo "REPEAT"
	  	  ((org-agenda-overriding-header "重复任务")))
	    (todo "CALENDAR"
	  	  ((org-agenda-overriding-header "日程表")))))

	  
	  ;; ("d" "Day" agenda "" ((org-agenda-span 1) ;limits display to a single day
	  ;; 			(org-agenda-sorting-strategy
	  ;; 			 (quote ((agenda time-up priority-down tag-up) )))
	  ;; 			(org-deadline-warning-days 0)
	  ;; 			;;(org-agenda-remove-tags t) ;don't show tags
	  ;; 			))

	  ;; ("w" . "Work")
	  ;; ("wn" "Next" tags-todo "+CATEGORY=\"work\"")
	  ;; ("ww" "Waiting" tags-todo "+TODO=\"WAITING\"+CATEGORY=\"work\"")

	  ;; ("p" . "Private")
	  ;; ("pn" "Next" tags-todo "-CATEGORY=\"work\"-CATEGORY=\"work\"-CATEGORY=\"train\"-CATEGORY=\"thinking\"") ;;-表示排除
	  ;; ("pw" "Waiting" tags-todo "+TODO=\"WAITING\"-CATEGORY=\"work\"-CATEGORY=\"train\"-CATEGORY=\"thinking\"")

	  ;; can use org-agenda T also
	  ;; ("n" "All Next" todo "NEXT")
	  ;; ("v" "All Waiting" todo "WAITING")
	  ;; ("D" "Agenda(week) + Next" ((agenda) (todo "NEXT")))
	  ;; ("z" "Waiting" todo "WAITING")

	  ;; ("g" "Goals" todo "GOAL")
	  
	  ;; ("g" "Weekly Goals Review"
	  ;;  ((tags "Goal=\"Long\""
	  ;; 	  ((org-agenda-overriding-header "Long term goals")))
	  ;;   (tags "Goal=\"Medium\""
	  ;; 	  ((org-agenda-overriding-header "Medium term goals")))
	  ;;   (tags "Goal=\"Short\""
	  ;; 	  ((org-agenda-overriding-header "Short term goals")))
	  ;;   (tags-todo "Goal=\"\""
	  ;; 	       ((org-agenda-overriding-header "Actions that don't contribute to a goal yet"))))
	  ;;  )

	  ;; ("G" "Goals" tags "GOAL-TODO=\"GOAL\"" nil)	  
	  ))

  (defun eye/open-agenda-day-view ()
    (interactive)
    (org-agenda nil "d"))

  ;; (add-hook 'after-init-hook #'eye/open-agenda-day-view)

  ;; 模板中的file路径不是绝对路径时，将会使用org-directory进行查找
  (setq org-directory locale-notebook-dir)

  ;; capture 的目标路径不能直接使用 concat
  ;; (defconst my-inbox-path (expand-file-name "Inbox.org" locale-notebook-dir))
  (setq my-todo-path "h:/wikinote/ats.org")
  ;; (defconst my-priv-todo-path (expand-file-name "Todo.org" locale-notebook-dir))
  ;; (defconst my-proj-todo-path (expand-file-name "Todo -- seo.org" locale-notebook-dir))
  ;; (defconst my-journal-path (expand-file-name "private/journal.org" locale-notebook-dir))
  ;; (defvar my-journal-time-format "%R") ;; like "%H:%M"
  (setq org-capture-templates
	'(
	  ;; ;; Web capture org-protocol: https://github.com/sprig/org-capture-extension
          ;; ("p" 
          ;;  "org-protocol(web link)" entry (file+headline my-todo-path "Inbox")
          ;;  "* [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]] %^G\n:PROPERTIES:\n:Created: %U\n:END:\n\n%i\n%?"
          ;;  :create t)
	  
          ;; ("L"
          ;;  "org-protocol(web content)" entry (file+headline my-inbox-path "Inbox")
          ;;  "* [[%:link][%:description]]\n%?\n"
          ;;  :create t)
	  ;; ("l" "Protocol Link" entry (file+headline my-todo-path "Inbox")
          ;; "* [[%:link][%:description]] \nCREATED: %u" :prepend t :immediate-finish t)
	  
	  ;; Capture information
          ("i" "Inbox" entry (file+headline my-todo-path "gtd")
           "* INBOX %?\n%i\n" :create t :empty-lines-after 1)

	  ("w" "Rx" entry (file+headline my-todo-path "rx")
           "* TASK %?\n%i\n" :create t :empty-lines-after 1)
	   
	  ;; Record event
	  ;; ("j" "Journal entry" entry (function org-journal-find-location)
           ;; "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")

	  ;; Work task
	  ;; file+olp specifies to full path to fill the Template
	  ;; ("w" "Work")
	  ;; ("wp" "Project(rx)" entry (file+olp my-todo-path "Projects" "rx")
	   ;; "* TODO %^{heading}\n:PROPERTIES:\n:CREATED: %U\n:END:")

	  ;; Private task
	  ;; file+olp specifies to full path to fill the Template
	  ;; ("m" "My private")
	  ;; ("mt" "Todo" entry (file+olp my-todo-path "Tasks")
           ;; "* TODO %^{heading} %^G\n:PROPERTIES:\n:CREATED: %U\n:END:")
	  ;; ("mp" "Project(so)" entry (file+olp my-todo-path "Projects" "so")
           ;; "* TODO %^{heading} %^G\n:PROPERTIES:\n:CREATED: %U\n:END:")
	  
	  ;; Create goal
	  ;; ("g" "Goals") 
	  ;; ("ge" "Epic goals" entry (file+headline "goals.org" 
	  ;; 					  "Epic goals") (file "tpl-goals.txt") :empty-lines-after 1) 
	  ;; ("gl" "Long term goal (2-5 years from now)" entry (file+headline "goals.org"
	  ;; 								   "Long term goals (2-5 years from now)") (file "tpl-goals.txt") :empty-lines-after 1) 
	  ;; ("gm" "Medium term goal (6 months up to 2 years)" entry (file+headline "goals.org"
	  ;; 									 "Medium term goalsl (6 months up to 2 years)") (file "tpl-goals.txt") :empty-lines-after 1) 
	  ;; ("gs" "Short term goals (next 6 months)" entry (file+headline "goals.org" 
	  ;; 								"Short term goals (next 6 months)") (file "tpl-goals.txt") :empty-lines-after 1)
          ))
  
;;;; Notebook
  (require-maybe 'org-attach)
  (add-to-list 'org-modules 'org-attach)
  (setq org-attach-directory "~/org/attach")
  )


;;;; 时间统计
(defvar eye-calc-time-tags '("train" "english" "work" "learning" "other"))

(defvar eye--current-tag nil)

;; 过滤headline，返回nil表示排除
(defun filter-by-tags ()
   (let ((head-tags (org-get-tags-at)))
     (member eye--current-tag head-tags)))

(defun org-clock-sum-today-by-tags (timerange &optional tstart tend noinsert)
  (interactive "P")
  (let* ((timerange-numeric-value (prefix-numeric-value timerange)) ;得到前缀C-u时的数字值
         ;; (files (org-add-archive-files (org-agenda-files)))
	 (files (directory-files locale-notebook-dir t "Journal-.*.org$")) ;只统计日志文件中的
         (tags-time-alist (mapcar (lambda (tag) `(,tag . 0)) eye-calc-time-tags)) ;关联列表tag和时间，单位是分钟
         (output-string "")
         (tstart (or tstart
		     ;;4表示按了一次C-u，统计前一天，开始时间减去一天的秒数。org-time-today返回的是当天开始的时间戳，减去86400秒则是前一天的开始时间
                     (and timerange (equal timerange-numeric-value 4) (- (org-time-today) 86400))
                     (and timerange (equal timerange-numeric-value 16) (org-read-date nil nil nil "Start Date/Time:"))
		     ;; 没有使用C-u时，默认计算今天
                     (org-time-today)))
         (tend (or tend
                   (and timerange (equal timerange-numeric-value 16) (org-read-date nil nil nil "End Date/Time:"))
		   ;; 没有使用C-u时，默认计算今天
                   (+ tstart 86400)))
         h m file item prompt donesomething)

    ;; (append-to-list 'files (directory-files locale-notebook-dir t "Journal.*.org$"))

    ;; 遍历所有agenda files，根据tag和时间范围统计时间
    (while (setq file (pop files))
      ;; 设置agenda buffer
      (setq org-agenda-buffer (if (file-exists-p file)
                                  (org-get-agenda-file-buffer file)
                                (error "No such file %s" file)))
      (with-current-buffer org-agenda-buffer
        (dolist (eye--current-tag eye-calc-time-tags) ;由于org-clock-sum的filter function必须是0参数，需要先声明一下eye--current-tag
          (org-clock-sum tstart tend 'filter-by-tags) ;根据tag统计时间，包含子节点
          (setcdr (assoc eye--current-tag tags-time-alist)
		  ;; org-clock-file-total-minutes保存了当前buffer内容的分钟数，加上原来的时间
                  (+ org-clock-file-total-minutes (cdr (assoc eye--current-tag tags-time-alist)))))))
    ;; 统计时间完成，准备输出
    (while (setq item (pop tags-time-alist))
      (unless (equal (cdr item) 0)	;只输入不为0时间的tag
        (setq donesomething t)
        (setq h (/ (cdr item) 60)	;计算小时
              m (- (cdr item) (* 60 h))) ;计算分钟
        (setq output-string (concat output-string (format "[-%s-] %.2d:%.2d\n" (car item) h m)))))
    (unless donesomething
      (setq output-string (concat output-string "[-Nothing-] Done nothing!!!\n")))
    (unless noinsert
        (insert output-string))
    output-string))



;; (defun eye/open-journal-file ()
  ;; (interactive)
  ;; (if (file-exists-p my-journal-path)
      ;; (find-file my-journal-path)
    ;; (message "No journal file.")))

(defun eye/open-attach-dired ()
  "显示对应的附件目录在下方"
  (interactive)
  (when (eq major-mode 'org-mode)
    (let ((h (- (window-body-height (selected-window)) 10))
	  (dir (org-attach-dir)))
      (message dir)
      (split-window-vertically h)
      (other-window 1)
      (dired dir)
      (other-window 1))))

(defun eye/notes-search-keyword ()
  (interactive)
  (let ((keyword (read-string "Search note keyword: " (eye/current-word))))
    (counsel-rg keyword locale-notebook-dir)
    ))

(defun eye/notes-search-file ()
  (interactive)
  (let ((keyword (read-string "Search note file: ")))
    (dired (expand-file-name "tecs" locale-notebook-dir))
    (if (fboundp 'swiper)
	(swiper keyword)
      (isearch-forward keyword))
    ))

(defun eye/notes-dired ()
  (interactive)
  (dired (expand-file-name "tecs" locale-notebook-dir)))

(defun eye/notes-new ()
  (interactive)
  (require 'org)
  (let ((name (read-string "New note(no suffix): "))
	(timestr (format-time-string "%Y-%m-%dT%H.%M.%S" (current-time)))
	(tags (read-string "tags:"))
	(dir (expand-file-name "tecs" locale-notebook-dir)))
    (when name
      (find-file (expand-file-name
		  (format "%s %s -- %s.org" timestr name tags)
		  dir))
      (set-buffer-file-coding-system 'utf-8-with-signature-unix 't) ;; 设置编码
      (insert (concat "* " name)) ;; 添加一级标题
      (org-attach-set-inherit)	; use same attach directory
      (org-set-tags-to (replace-regexp-in-string " " ":" tags))
      )))

(defun eye/clean-attach-dir ()
  "删除空的附件文件夹"
  (interactive)
  (let ((files (delete "." (delete ".." (directory-files org-attach-directory t))))
	(count 0))
    (mapcar (lambda (dir)
	      (when (f-empty? dir)
		(f-delete dir t)
		(setq count (1+ count))))
	    files)
    (message "clean finished, %s empty dir removed." count)
    ))

(defun eye/insert-attach-link ()
  "添加一个附件链接"
  (interactive)
  ;; scan ort-attach-dir
  (message (org-attach-dir))
  ;; select a file
  (let* ((attach-dir (org-attach-dir))
	 (files (delete "." (delete ".." (directory-files attach-dir))))
	 choice fullpath newname tags link temp)
    (when files
      (setq choice (ido-completing-read "file:" files))
      (when choice
	(setq fullpath (expand-file-name choice attach-dir))
	;; ask change file name
	(if (y-or-n-p "change file name(y/n)?")
	    (progn
	      (setq temp (read-string "change name to:" (file-name-base choice)))
	      (setq tags (read-string "add tag:"))
	      (setq newname (format "%s %s -- %s.%s"
				    (format-time-string "%Y-%m-%dT%H.%M.%S" (current-time))
				    temp
				    tags
				    (file-name-extension choice)))
	      (rename-file fullpath (expand-file-name newname attach-dir))
	      (setq fullpath (expand-file-name newname attach-dir))
	      )
	  (setq newname choice))
	;; get relative path
	(setq link (format "[[file:%s][%s]]"
			   (file-relative-name fullpath)
			   newname))
	;; insert link
	(insert link)	
	))
      )
  )

;; 使用org-attache方式管理附件，不再需要下面的函数
;; (defun eye/notes-create-attachment ()
;;   "创建文件对应的附件文件夹"
;;   (interactive)
;;   (let* ((name (replace-regexp-in-string ".org" "" (buffer-name)))
;; 	   (dir (concat locale-notebook-attachment-dir "/" name)))
;;     (unless (f-directory? locale-notebook-attachment-dir) (f-mkdir locale-notebook-attachment-dir)) ;; 创建附件主目录
;;     (unless (f-directory? dir) ;; 创建附件子目录
;; 	(f-mkdir dir))
;;     ))

;; (defun eye/notes-open-attachment ()
;;   "打开与当前文件名相同的附件文件夹"
;;   (interactive)
;;   (let* ((name (replace-regexp-in-string ".org" "" (buffer-name)))
;; 	   (dir (concat locale-notebook-attachment-dir "/" name)))
;;     (if (f-directory? dir)
;; 	  (progn
;; 	    (shell-command (concat "explorer "
;; 				   (encode-coding-string
;; 				    (replace-regexp-in-string "/" "\\\\" dir) 'gbk-dos))) ;; 转换为windows路径后再转换为gbk编码，否则无法打开中文目录
;; 	    )
;; 	(message "Attachment folder not exists!")	
;; 	)))


(defun eye-journal-find-location ()
  (expand-file-name (format-time-string "%Y-%m-%d.org")
		    eye-journal-dir))

;; (defun eye-journal-find-end ()
;;   (find-file (eye-journal-find-location))
;;   (goto-char (point-max)))


(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  ;; Position point on the journal's top-level heading so that org-capture
  ;; will add the new entry as a child entry.
  (goto-char (point-min)))
	     
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  ;; (org-journal-new-entry t)
  ;; Position point on the journal's top-level heading so that org-capture
  ;; will add the new entry as a child entry.
  ;; (goto-char (point-max)))

(defun eye/open-journal-file ()
  (interactive)
  (let* ((path (eye-journal-find-location))
	 (iscreate (not (file-exists-p path))))
    (when path
      ;; (setq today-journal-path path)
      (find-file path)
      ;; goto today entry
      ;; (search-forward (format-time-string "* %Y-%m-%d") nil t)
      ;; goto end
      (end-of-buffer)
      )))

  
  
(with-eval-after-load 'org
  (eye-setup-orgmode)
  ;; (eye-set-leader-key org-mode-map)
  ;; (with-eval-after-load 'org-agenda
    ;; (eye-set-leader-key org-agenda-mode-map))
  )




(provide 'init-orgmode)
