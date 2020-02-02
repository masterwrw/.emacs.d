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
;; 使用org-mode9.3版本
;; 带中文的链接不会被转成百分号
;; 附件就在当前位置插入
;;(eh-hack-load-path)
;;(autoload 'org-version "org" "" t) ;; fix yankpad-insert error if use org9.3
;;(add-to-list 'load-path (expand-file-name "org-mode/lisp" "~/src/emacs-packages"))

;; 9.3使用<s需要org-tempo
;;(require 'org-tempo)

;; 快速添加 src block，使用 <el 加 tab 键
;;(add-to-list 'org-structure-template-alist '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))
;;(add-to-list 'org-structure-template-alist '("cpp" "#+BEGIN_SRC C++\n?\n#+END_SRC"))
;;(add-to-list 'org-structure-template-alist '("lu" "#+BEGIN_SRC lua\n?\n#+END_SRC"))
;;(add-to-list 'org-structure-template-alist '("py" "#+BEGIN_SRC python\n?\n#+END_SRC"))

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


(setq gtd-inbox-path (concat locale-notebook-dir "/gtd/inbox.org"))     ;; 收集所有东西
(setq gtd-someday-path (concat locale-notebook-dir "/gtd/someday.org"))      ;; 不确定什么时候做或者以后要做的事
(setq gtd-gtd-path (concat locale-notebook-dir "/gtd/gtd.org"))              ;; gtd主文件
(setq gtd-tickler-path (concat locale-notebook-dir "/gtd/tickler.org"))      ;; 需要提醒的事项
(setq gtd-archive-path (concat locale-notebook-dir "/gtd/archive-2019.org")) ;; 归档文件

;;;; org-agenda
(defun setup-org-agenda ()
  ;; 设置所有以Todo开头的org文件
  ;; (setq org-agenda-files (directory-files locale-notebook-dir t "Todo.*.org$"))
  ;; 2019-12-06参考https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
  (setq org-agenda-files `(,gtd-inbox-path
			   ,gtd-gtd-path
			   ,gtd-tickler-path))
  ;; full frame show
  (setq org-agenda-window-setup 'only-window)
  ;; (setq org-agenda-block-separator nil)
  (setq org-agenda-align-tags-to-column 1) ;在agenda视图中，使tags向左边对齐，默认是auto，向右边对齐，会换行显示
  (setq org-agenda-deadline-leaders (quote ("最后期限:  " "%3d 天后到期: " "%2d 天前: "))
	org-agenda-scheduled-leaders (quote ("计划任务:" "计划任务(第%2d次激活): "))
	org-agenda-inhibit-startup t
	org-agenda-span 'day
	)

  
  (setq-default org-agenda-format-date (quote my-org-agenda-format-date-aligned))
  (defun my-org-agenda-format-date-aligned (date)
    "Format a DATE string for display in the daily/weekly agenda.
This function makes sure that dates are aligned for easy reading."
    (require 'cal-iso)
    (let* ((dayname (calendar-day-name date))
	   (day (cadr date))
	   (day-of-week (calendar-day-of-week date))
	   (month (car date))
	   (monthname (calendar-month-name month))
	   (year (nth 2 date))
	   (iso-week (org-days-to-iso-week
		      (calendar-absolute-from-gregorian date)))
	   (weekyear (cond ((and (= month 1) (>= iso-week 52))
			    (1- year))
			   ((and (= month 12) (<= iso-week 1))
			    (1+ year))
			   (t year)))
	   (weekstring (if (= day-of-week 1)
			   (format " W%02d" iso-week)
			 "")))
      ;; 修改点：在agenda中显示的日期格式
      (format "%4d-%s-%02d %-4s %s"
	      year monthname day dayname weekstring)))

  ;; 只查看一个需要做的任务
  (setq org-agenda-custom-commands 
	'(("o" "project" tags-todo "proj" ;; 搜索tag
	   ((org-agenda-overriding-header "project") ;;用于显示的字符串
	    (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))))

  ;; 跳过后面的todo项，只显示第一个
  (defun my-org-agenda-skip-all-siblings-but-first ()
    "Skip all but the first non-done entry."
    (let (should-skip-entry)
      (unless (org-current-is-todo)
	(setq should-skip-entry t))
      (save-excursion
	(while (and (not should-skip-entry) (org-goto-sibling t))
	  (when (org-current-is-todo)
	    (setq should-skip-entry t))))
      (when should-skip-entry
	(or (outline-next-heading)
	    (goto-char (point-max))))))
  
  (defun org-current-is-todo ()
    (string= "TODO" (org-get-todo-state)))


  ;; (setq org-agenda-custom-commands
  ;; 	'(
  ;; 	  ("i" "View inbox" todo "INBOX")
  ;; 	  ("t" "View tasks" todo "TASK|SOMEDAY|REPEAT|CALENDAR")
  ;; 	  ("o" "View todo" todo "TODO")
  ;; 	  ("x" "View action" todo "ACTION")

  ;; 	  ("z" "View tasks"
  ;; 	   ((todo "TASK"
  ;; 		  ((org-agenda-overriding-header "任务")))
  ;; 	    (todo "SOMEDAY"
  ;; 		  ((org-agenda-overriding-header "将来/也许")))
  ;; 	    (todo "REPEAT"
  ;; 		  ((org-agenda-overriding-header "重复任务")))
  ;; 	    (todo "CALENDAR"
  ;; 		  ((org-agenda-overriding-header "日程表")))))

  )

;;;; org
(auto-require 'org
	      :paths '("emacs-htmlize" "org-mode")
	      :after
	      (progn
		(require 'org-capture)
		(require 'org-agenda)
		(require 'htmlize)
		(require 'ob-shell)
		(setq system-time-locale "C")
		;;%a表示插入时间时显示“周几”，如果没有设置system-time-locale为"C"的话，会显示乱码
		(setq org-time-stamp-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M>"))
		(setq org-enforce-todo-dependencies t)
		(setq org-ellipsis " ")
		(setq org-src-fontify-natively t) ;; 代码块内语法高亮
		(setq org-src-tab-acts-natively t)
		;; (setq org-src-window-setup 'current-window) ;; 在当前window打开src block
		;; (add-hook 'org-mode-hook 'yas-minor-mode)
		;; indent content
		(setq org-edit-src-content-indentation 0) ;; 代码块默认不缩进
		(setq org-startup-indented nil) ;; 是否自动开启org-indent-mode
		(setq org-startup-folded (quote overview))
		;; hides blank lines between headings
		(setq org-cycle-separator-lines 0)
		;; always require new line in header below
		(setq require-final-newline t)
		(setq org-tags-column 0)		;; 在org文件中，使tags跟在标题后面
		(setq org-return-follows-link t) ;; 是否回车打开link
		(setq org-clock-string "计时:"
		      org-closed-string "已关闭:"
		      org-deadline-string "最后期限:"
		      org-scheduled-string "计划任务:"
		      org-time-stamp-formats  '("<%Y-%m-%d 周%u>" . "<%Y-%m-%d 周%u %H:%M>")
		      org-deadline-warning-days 5	;最后期限到达前5天即给出警告
		      org-log-done 'time
		      org-link-file-path-type  'relative ;插入链接时使用相对路径
		      org-log-done 'time		 ;完成时添加时间
		      ;; code执行免应答（Eval code without confirm）
		      org-confirm-babel-evaluate nil
		      )


		(setup-org-agenda)
		
		(setq-default
		 calendar-date-style 'iso
		 calendar-day-abbrev-array ["周日" "周一" "周二" "周三" "周四" "周五" "周六"]
		 calendar-day-name-array ["周日" "周一" "周二" "周三" "周四" "周五" "周六"]
		 ;;calendar-month-name-array ["一月" "二月" "三月" "四月" "五月" "六月" "七月" "八月" "九月" "十月" "十一月" "十二月"]
		 calendar-month-name-array ["01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12"]
		 calendar-week-start-day 1 ;; 日历从周一开始显示
		 )
		
		(setq org-support-shift-select 1) ;; 是否支持shift+方向键选择
		(setq org-fontify-emphasized-text t) ;; 高亮行内代码标记等 https://orgmode.org/manual/Emphasis-and-Monospace.html#Emphasis-and-Monospace
		(setq org-hide-emphasis-markers t) ;; 隐藏斜体标记/text/，如果要删除，则确保光标移到斜体文字最后
		;; 模板中的file路径不是绝对路径时，将会使用org-directory进行查找
		(setq org-directory locale-notebook-dir)
		;; 用圆形符号表示列表开头，匹配" - "
		(font-lock-add-keywords 'org-mode
					'(("^ +\\([-*]\\) "
					   (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

		;; Speed keys, @see https://orgmode.org/manual/Speed-keys.html
		;; quick navigation when cursor is on a headline (before any of the stars)
		;; ?:for help, n/p/f/b...
		(setq org-use-speed-commands t)
		;; (setq org-todo-keywords
		;;       '((sequence "INBOX(i)")
		;; 	(sequence "TASK(t)" "SOMEDAY(s)" "REPEAT(r)" "CALENDAR(c)")
		;; 	(sequence "TODO(T)" "ACTION(a)" "WAIT(w)" "DONE(d!)" "|" "CANCELLED(C)" "DEFERRED(f)")
		;; 	(sequence "PLAN(p)")
		;; 	))
		(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
		(setf org-todo-keyword-faces
		      '(;;("INBOX" . (:foreground "OliveDrab" :bold t :weight bold))
			;;("TASK" . (:foreground "OrangeRed" :bold t :weight bold))
			;;("SOMEDAY" . (:foreground "chocolate" :bold t :weight bold))
			;;("REPEAT" . (:foreground "#009900" :bold t :weight bold))
			;;("ACTION" . (:foreground "cyan" :bold t :weight bold))
			("TODO" . (:foreground "red" :bold t :weight bold))
			("WAITING" . (:foreground "DarkRed" :bold t :weight bold))
			("DONE" . (:foreground "green" :bold t :weight bold))
			("CANCELLED" . (:foreground "gray50" :bold t :weight bold))
			))
		;; tags
		;; #+TAGS: { @work(w) @life(l) @thinking(t) @study(s) }
		;; #+TAGS: { @add(a) @bug(b) @fixed(f) }
		(setq org-tag-alist '(("ARCHIVE" . ?a) ("work" . ?w) ("learn" . ?l) ("english" . ?e) ("brain" . ?t) ("body" . ?b) ("other" . ?o)))
		;; column view format, can write "#+COLUMNS: ..." to org file also 
		;; (setq org-columns-default-format "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent)  %DEADLINE(Deadline)")
		(setq org-columns-default-format "%7Goal(Goal) %12DEADLINE(Deadline) %30SCHEDULED(Scheduled) %8TODO(To Do) %1PRIORITY(P) %150ITEM(Detailes)")

		(setq org-default-notes-file (expand-file-name "inbox.org" locale-notebook-dir))
		(defun eye/open-inbox-file () (interactive) (find-file org-default-notes-file))
		;; C-c C-w: org-refile 从inbox移到其它文件，不需要再移回inbox文件
		(setq org-refile-targets
		      `((,gtd-gtd-path :maxlevel . 3)     ;; 最多第3层
			(,gtd-someday-path :level . 1)    ;; 只要第1层
			(,gtd-tickler-path :maxlevel . 2) ;; 最多第2层
			))

		;; org-archive-subtree moving an tree to archive file
		;; settings on org file #+ARCHIVE file head or ARCHIVE PROPERTY
		(setq org-archive-location (concat gtd-archive-path "::"))
;;		(defalias 'org-beginning-of-line nil) ;
		;; (defun eye/org-meta-return ()
		;;   "确保按下M-RET时不会打断当前行（但是折叠有属性或内容时会打断属性）"
		;;   (interactive)
		;;   (org-end-of-line)
		;;   (org-meta-return))
		(define-key org-mode-map (kbd "M-RET") 'org-insert-heading-respect-content)		
		;; Line wrapping
		(add-hook 'org-mode-hook
			  '(lambda ()
			     (visual-line-mode 1)))

		(global-set-key (kbd "C-c '") 'org-edit-src-code)
		

		(add-hook 'org-mode-hook (lambda ()
					   "Beautify org symbols."
					   (push '("[ ]" . ?☐) prettify-symbols-alist)
					   (push '("[X]" . ?☑) prettify-symbols-alist)
					   (push '("[-]" . ?⛝) prettify-symbols-alist)

					   (push '("#+ARCHIVE:" . ?📦) prettify-symbols-alist)
					   (push '("#+AUTHOR:" . ?👤) prettify-symbols-alist)
					   (push '("#+CREATOR:" . ?💁) prettify-symbols-alist)
					   (push '("#+DATE:" . ?📆) prettify-symbols-alist)
					   (push '("#+DESCRIPTION:" . ?🗎) prettify-symbols-alist)
					   (push '("#+EMAIL:" . ?🖂) prettify-symbols-alist)
					   (push '("#+OPTIONS:" . ?⚙) prettify-symbols-alist)
					   (push '("#+TAGS:" . ?🏷) prettify-symbols-alist)
					   (push '("#+TITLE:" . ?🕮) prettify-symbols-alist)

					   (push '("#+BEGIN_SRC" . ?✎) prettify-symbols-alist)
					   (push '("#+END_SRC" . ?□) prettify-symbols-alist)
					   (push '("#+BEGIN_QUOTE" . ?») prettify-symbols-alist)
					   (push '("#+END_QUOTE" . ?«) prettify-symbols-alist)
					   (push '("#+HEADERS" . ?☰) prettify-symbols-alist)
					   (push '("#+RESULTS:" . ?💻) prettify-symbols-alist)))
		(prettify-symbols-mode 1)

		))


(auto-require 'prettify-saymbols-mode
	      :reqby 'org)

;;;; htmlize
;; Exported to HTML
(auto-require 'htmlize
	      :paths "emacs-htmlize"
	      :reqby 'org)


;;;; ob-shell
;; support babel execute
(auto-require 'ob-shell
	      :reqby 'org
	      :after
	      (progn
		(org-babel-do-load-languages
		 'org-babel-load-languages '((emacs-lisp . t)
					     (shell . t)))))

;;;; org-capture
(auto-require 'org-capture
	      :reqby 'org
	      :functions 'org-capture
	      :before
	      (progn
		;; capture 的目标路径不能直接使用 concat
		(setq org-capture-templates '(("i" "Todo [收集]" entry
					       (file+headline gtd-inbox-path "Inbox")
					       "* TODO %i%?")
					      ("T" "Tickler [提醒]" entry
					       (file+headline gtd-tickler-path "Tickler")
					       "* %i%? \n %U")
					      
					      ))
		))


(auto-require 'find-lisp :reqby 'org)

;; for task more times repeat a week.
;; @see https://stackoverflow.com/questions/8751287/weekly-repeating-tasks-emacs-org-mode
;;(require 'org-install)
;;(add-to-list 'org-modules 'org-habit)

;; System locale to use for formatting time values.
;; current is "zh_CN.UTF-8", if set to "C", Make sure that the weekdays in the
;; time stamps of your Org mode files and in the agenda appear in English.
;; @see https://github.com/batsibe/org-journal
(auto-require 'org-journal
	      :paths "org-journal"
	      :reqby 'org
	      :functions 'org-journal-new-entry
	      :after
	      (progn
		;;(setq org-journal-date-format 'org-journal-date-format-func)
		(setq org-journal-date-format "%Y-%m-%d, %u")

		(setq org-journal-file-type 'yearly)
		(setq org-journal-dir (concat locale-notebook-dir "/journal/"))
		(setq org-journal-file-format "%Y.org")
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
		))


(auto-require 'org-protocol :reqby 'org)

(defun transform-square-brackets-to-round-ones(string-to-transform)
  "Transforms [ into ( and ] into ), other chars left unchanged."
  (concat 
   (mapcar #'(lambda (c) (if (equal c ?[) ?\( (if (equal c ?]) ?\) c))) string-to-transform))
  )


;;; custom agenda command
;; method 1
;; (setq eye/gtd-someday-view
;;       `("U" "someday" todo "SOMEDAY"
;;         ((org-agenda-files (list locale-gtd-task)))))
;; (add-to-list 'org-agenda-custom-commands `,eye/gtd-someday-view)
;; method 2

;;;; simple gtd
(setq eye-journal-dir (concat locale-notebook-dir "/gtd/journal"))

(defun eye-journal-find-location ()
  (expand-file-name (format-time-string "%Y-%m-%d.org")
		    eye-journal-dir))

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


(provide 'init-orgmode)
