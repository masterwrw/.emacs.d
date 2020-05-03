;;;; org
(require 'org)


(setq gtd-inbox-path (concat locale-notebook-dir "/gtd/inbox.org"))          ;; 收集所有东西
(setq gtd-someday-path (concat locale-notebook-dir "/gtd/someday.org"))      ;; 不确定什么时候做或者以后要做的事
(setq gtd-gtd-path (concat locale-notebook-dir "/gtd/gtd.org"))              ;; gtd主文件
(setq gtd-tickler-path (concat locale-notebook-dir "/gtd/tickler.org"))      ;; 需要提醒的事项
(setq gtd-archive-path (concat locale-notebook-dir "/gtd/archive-2019.org")) ;; 归档文件

;; 9.3使用<s需要org-tempo
(when (string-equal (org-version) "9.3")
  (require 'org-tempo))


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
(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "DEFERRED(f)" "|" "DONE(d)" "SOMEDAY(s)" "CANCELLED(c)")))
(setf org-todo-keyword-faces
      '(("TODO" . (:foreground "red" :bold t :weight bold))
	("WAITING" . (:foreground "DarkRed" :bold t :weight bold))
	("DEFERRED" . (:foreground "red" :bold t :weight bold))
	("DONE" . (:foreground "green" :bold t :weight bold))
	("SOMEDAY" . (:foreground "gray30" :bold t :weight bold))
	("CANCELLED" . (:foreground "gray50" :bold t :weight bold))
	))

;; org-archive-subtree moving an tree to archive file
;; settings on org file #+ARCHIVE file head or ARCHIVE PROPERTY
(setq org-archive-location (concat gtd-archive-path "::"))

;; C-c C-w: org-refile 从inbox移到其它文件，不需要再移回inbox文件
(setq org-refile-targets
      `((,gtd-gtd-path :maxlevel . 1)     ;; 最多第1层
	(,gtd-someday-path :level . 1)    ;; 只要第1层
	(,gtd-tickler-path :maxlevel . 1) ;; 最多第1层
	))


(setq org-default-notes-file (expand-file-name "todo.today.org" locale-notebook-dir))

(define-key org-mode-map (kbd "M-RET") 'org-insert-heading-respect-content)
(define-key org-mode-map (kbd "C-k") nil)

;; Line wrapping
(add-hook 'org-mode-hook (lambda () (visual-line-mode 1)))

(global-set-key (kbd "C-c '") 'org-edit-src-code)



;;;; calendar
(setq-default
 calendar-date-style 'iso
 calendar-day-abbrev-array ["周日" "周一" "周二" "周三" "周四" "周五" "周六"]
 calendar-day-name-array ["周日" "周一" "周二" "周三" "周四" "周五" "周六"]
 ;;calendar-month-name-array ["一月" "二月" "三月" "四月" "五月" "六月" "七月" "八月" "九月" "十月" "十一月" "十二月"]
 calendar-month-name-array ["01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12"]
 calendar-week-start-day 1 ;; 日历从周一开始显示
 )


;;;; htmlize
(add-to-list 'load-path (concat auto-require-packages-dir "/emacs-htmlize"))
(require 'htmlize)

;;;; ob-shell
;; support babel execute
(require 'ob-shell)
(org-babel-do-load-languages
 'org-babel-load-languages '((emacs-lisp . t)
			     (shell . t)))
;;------------------------------------------------------
;;;; org-capture
;;------------------------------------------------------
(require 'org-capture)
;; capture 的目标路径不能直接使用 concat

(defun my-org-journal-find-location ()
  "以当前日期为文件名，第一级也是当前日期，属性为diary"
  (let* ((today (format-time-string "%Y-%m-%d" (current-time)))
	(today-file (concat locale-notebook-dir "/journal/" today ".org")))
    (find-file today-file)
    (goto-char (point-min)) ;; 防止重复创建日期节点
    (if (search-forward today nil t)
	(goto-char (point-max))
      (progn
	(goto-char (point-max))
	(insert (concat "* " today "\n:PROPERTIES:\n:CATEGORY: diary\n:END:\n")))
      )))


;; Inbox
(add-to-list 'org-capture-templates '("i" "Inbox [收集]" entry (file+headline gtd-inbox-path "Inbox")
				      "* TODO %i%?"))


;; Tickler
;; %^t 输入提醒时间
(add-to-list 'org-capture-templates '("t" "Tickler [提醒]" entry (file+headline gtd-tickler-path "Tickler")
				      "* %^t %i%? \n"))

;; 日记模板
;; %T  插入时间戳，便于在agenda中显示
;; %^G 插入tags
;; 问题：**无法创建二级标题，需要手动调整一下
(add-to-list 'org-capture-templates '("j" "Journal" entry (function my-org-journal-find-location)
				      "* %T %^{Title} %^G\n%i%?"))


;; TODO
(defun my/process-daily-item ()
  "根据GTD流程自动处理inbox item"
  (interactive)
  (when (y-or-n-p "Is is Actionable?")
    (org-todo)
    (if (y-or-n-p "A single step(if not, move to a project)?")
      (org-refile))))



;;------------------------------------------------------
;;;; org-agenda
;;------------------------------------------------------
(require 'org-agenda)

;; 每三个小时为一间隔
(setq org-agenda-time-grid (quote ((daily today require-timed)
				   (300 600 900 1200 1500 1800 2100 2400)
				   "......" "------------------")))

(setq org-agenda-files `(,gtd-inbox-path
			 ,gtd-gtd-path
			 ,gtd-someday-path
			 ,gtd-tickler-path))

(add-to-list 'org-agenda-files (concat locale-notebook-dir "/journal/"))

  
  
  
;; full frame show
(setq org-agenda-window-setup 'only-window)
;; (setq org-agenda-block-separator nil)
(setq org-agenda-align-tags-to-column 1) ;在agenda视图中，使tags向左边对齐，默认是auto，向右边对齐，会换行显示
(setq org-agenda-deadline-leaders (quote ("最后期限:  " "%3d 天后到期: " "%2d 天前: "))
      org-agenda-scheduled-leaders (quote ("计划任务:" "计划任务(第%2d次激活): "))
      org-agenda-inhibit-startup t
      org-agenda-span 'day)


;; 自定义日期显示格式
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



(add-to-list 'org-agenda-custom-commands '("i" "View inbox" tags-todo "inbox"
					   ((org-agenda-overriding-header "Inbox"))))

(add-to-list 'org-agenda-custom-commands '("t" "View personal todolist" tags-todo "task|repeat|body"
					   ((org-agenda-overriding-header "Task"))))
	 

;; 查看project rx
(add-to-list 'org-agenda-custom-commands '("p" . "View project todolist"))

(add-to-list 'org-agenda-custom-commands '("pr" "Rx" tags-todo "proj+rx" ;; 搜索tag
					   ((org-agenda-overriding-header "Rx")))) ;;用于显示的字符串

(add-to-list 'org-agenda-custom-commands '("ph" "Hisi" tags-todo "proj+hisi" ;; 搜索tag
					   ((org-agenda-overriding-header "Hisi")))) ;;用于显示的字符串

(add-to-list 'org-agenda-custom-commands '("pm" "Memory" tags-todo "proj+memory" ;; 搜索tag
					   ((org-agenda-overriding-header "Memory")))) ;;用于显示的字符串


;; 自动打开calendar
(advice-add 'org-agenda :after
	    (lambda (_)
	      (when (equal (buffer-name)
			   "*Org Agenda*")
		(calendar)
		(other-window 1))))

;; 自动退出calendar
(advice-add 'org-agenda-quit :before
		    (lambda ()
		      (let ((window (get-buffer-window calendar-buffer)))
			(when (and window (not (one-window-p window)))
			                    (delete-window window)))))



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




(provide 'init-orgmode)
