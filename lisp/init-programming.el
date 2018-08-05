(require-package 'dash)
(require 'dash)


;; ediff
(defun owen-ediff-setup-windows (buffer-A buffer-B buffer-C control-buffer)
  (ediff-setup-windows-plain buffer-A buffer-B buffer-C control-buffer)
  )
(setq ediff-window-setup-function 'owen-ediff-setup-windows)
(setq ediff-split-window-function 'split-window-horizontally)


;; imenu-list, Show imenu entries in a separate buffer
(require-package 'imenu-list)
(require 'imenu-list)

;;; Pair configuration
(show-paren-mode 1)
;; autopair
(require-package 'autopair)
(require 'autopair)
(autopair-global-mode)
(diminish 'autopair-mode)


;; change-inner
(require-package 'change-inner)
(require 'change-inner)


;; smartparens
(require-package 'smartparens)
(require 'smartparens)



;; Indent configuration
(setq tab-width 4 indent-tabs-mode nil)
(setq c-basic-offset 4 c-default-style "bsd")
;; dtrt-indent, Adapt to foreign indentation offsets
(require-package 'dtrt-indent)
(require 'dtrt-indent)
(dtrt-indent-mode 1)


;; aggressive-indent, Minor mode to aggressively keep your code always indented
(require-package 'aggressive-indent)
(require 'aggressive-indent)


;;indent-guide
(require-package 'indent-guide)
(indent-guide-global-mode)




;; auto-highlight-symbol
(require-package 'auto-highlight-symbol)
;(require 'auto-highlight-symbol)
;(global-auto-highlight-symbol-mode t) ;; Conflict with M-<left> for jump back.
(setq global-auto-highlight-symbol-mode nil)
(diminish 'auto-highlight-symbol-mode)





;; ws-butler, Unobtrusively remove trailing whitespace.
(require-package 'ws-butler)
(require 'ws-butler)
(add-hook 'prog-mode-hook 'ws-butler-mode)
(add-hook 'text-mode 'ws-butler-mode)
(add-hook 'fundamental-mode 'ws-butler-mode)
(diminish 'ws-butler-mode)


;; anzu, Show number of matches in mode-line while searching.
(require-package 'anzu)
(require 'anzu)
(global-anzu-mode)
(diminish 'anzu-mode)


;; clean-aindent-mode, Simple indent and unindent, trims indent white-space.
(require-package 'clean-aindent-mode)
(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)


;; volatile-highlights, Minor mode for visual feedback on some operations.
(require-package 'volatile-highlights)
(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)


;; iedit, Edit multiple regions in the same way simultaneously.
(require-package 'iedit)


;; yasnippet
(require-package 'yasnippet)
(require 'yasnippet)
(yas-global-mode 1)

(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")


;; Jump to previous position on current buffer.
(require 'jtpp)




(setq auto-mode-alist
      (append
       '(("\\.cpp$"    . c++-mode)
         ("\\.hin$"    . c++-mode)
         ("\\.cin$"    . c++-mode)
         ("\\.inl$"    . c++-mode)
         ("\\.rdc$"    . c++-mode)
         ("\\.h$"    . c++-mode)
         ("\\.c$"   . c++-mode)
         ("\\.cc$"   . c++-mode)
         ("\\.c8$"   . c++-mode)
         ("\\.txt$" . indented-text-mode)
         ("\\.emacs$" . emacs-lisp-mode)
         ("\\.gen$" . gen-mode)
         ("\\.ms$" . fundamental-mode)
         ("\\.m$" . objc-mode)
         ("\\.mm$" . objc-mode)
         ) auto-mode-alist))

					; C++ indentation style
(defconst owen-big-fun-c-style
  '((c-electric-pound-behavior   . nil)
    (c-tab-always-indent         . t)
    (c-comment-only-line-offset  . 0)
    (c-hanging-braces-alist      . ((class-open)
                                    (class-close)
                                    (defun-open)
                                    (defun-close)
                                    (inline-open)
                                    (inline-close)
                                    (brace-list-open)
                                    (brace-list-close)
                                    (brace-list-intro)
                                    (brace-list-entry)
                                    (block-open)
                                    (block-close)
                                    (substatement-open)
                                    (statement-case-open)
                                    (class-open)))
    (c-hanging-colons-alist      . ((inher-intro)
                                    (case-label)
                                    (label)
                                    (access-label)
                                    (access-key)
                                    (member-init-intro)))
    (c-cleanup-list              . (scope-operator
                                    list-close-comma
                                    defun-close-semi))
    (c-offsets-alist             . ((arglist-close         .  c-lineup-arglist)
                                    (label                 . -4)
                                    (access-label          . -4)
                                    (substatement-open     .  0)
                                    (statement-case-intro  .  4)
                                    (statement-block-intro .  c-lineup-for)
                                    (case-label            .  4)
                                    (block-open            .  0)
                                    (inline-open           .  0)
                                    (topmost-intro-cont    .  0)
                                    (knr-argdecl-intro     . -4)
                                    (brace-list-open       .  0)
                                    (brace-list-intro      .  4)))
    (c-echo-syntactic-information-p . t))
  "Owen's Big Fun C++ Style")


					; CC++ mode handling
(defun owen-big-fun-c-hook ()
					; Set my style for the current buffer
  (c-add-style "BigFun" owen-big-fun-c-style t)

					; 4-space tabs
  (setq tab-width 4
        indent-tabs-mode nil)

					; Additional style stuff
  (c-set-offset 'member-init-intro '++)

					; No hungry backspace
  (c-toggle-auto-hungry-state -1)

					; Newline indents, semi-colon doesn't

  (setq c-hanging-semi&comma-criteria '((lambda () 'stop)))

					; Handle super-tabbify (TAB completes, shift-TAB actually tabs)
  (setq dabbrev-case-replace t)
  (setq dabbrev-case-fold-search t)
  (setq dabbrev-upcase-means-case-search t)

					; Abbrevation expansion
  (abbrev-mode 1)


  (defvar current-date-time-format "%Y-%m-%d %H:%M:%S"
    "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

  (defvar current-time-format "%a %H:%M:%S"
    "Format of date to insert with `insert-current-time' func.
	Note the weekly scope of the command's precision.")

  (defun owen-insert-copyright ()
					;(system-time-locale "en_US")
    (setq BaseFileName (file-name-nondirectory buffer-file-name));(file-name-sans-extension (file-name-nondirectory buffer-file-name)))
    (insert "/* ========================================================================\n")
    (insert (concat "   $File: " BaseFileName " $\n"))
    (insert (concat "   $Date: " (format-time-string current-date-time-format (current-time)) " $\n"))
    (insert "   $Revision: 0001 $\n")
    (insert "   $Creator: OwenLang (owenlang@163.com) $\n")
    (insert "   $Notice: (C) Copyright 2016 by OwenLang $\n")
    (insert "   ======================================================================== */\n")
    )

  (defun owen-header-format ()
    "Format the given file as a header file."
    (interactive)
    (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
    (insert "#if !defined(")
    (push-mark)
    (insert BaseFileName)
    (upcase-region (mark) (point))
    (pop-mark)
    (insert "_H)\n")
    (owen-insert-copyright)
    (insert "\n")
    (insert "#define ")
    (push-mark)
    (insert BaseFileName)
    (upcase-region (mark) (point))
    (pop-mark)
    (insert "_H\n")
    (insert "#endif")
    )

  (defun owen-source-format ()
    "Format the given file as a source file."
    (interactive)
    (owen-insert-copyright)
    )

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]hin" buffer-file-name) (owen-source-format))
        ((string-match "[.]cin" buffer-file-name) (owen-source-format))
        ((string-match "[.]h" buffer-file-name) (owen-header-format))
        ((string-match "[.]cpp" buffer-file-name) (owen-source-format))
	((string-match "[.]c" buffer-file-name) (owen-source-format)))

  (defun owen-find-corresponding-file ()
    "Find the file that corresponds to this one."
    (interactive)
    (setq CorrespondingFileName nil)
    (setq BaseFileName (file-name-sans-extension buffer-file-name))
    (if (string-match "\\.c" buffer-file-name)
	(setq CorrespondingFileName (concat BaseFileName ".h")))
    (if (string-match "\\.h" buffer-file-name)
	(if (file-exists-p (concat BaseFileName ".c")) (setq CorrespondingFileName (concat BaseFileName ".c"))
	  (setq CorrespondingFileName (concat BaseFileName ".cpp"))))
    (if (string-match "\\.hin" buffer-file-name)
	(setq CorrespondingFileName (concat BaseFileName ".cin")))
    (if (string-match "\\.cin" buffer-file-name)
	(setq CorrespondingFileName (concat BaseFileName ".hin")))
    (if (string-match "\\.cpp" buffer-file-name)
	(setq CorrespondingFileName (concat BaseFileName ".h")))
    (if (string-match "\\.c" buffer-file-name)
	(setq CorrespondingFileName (concat BaseFileName ".h")))
    (if CorrespondingFileName (find-file CorrespondingFileName)
      (error "Unable to find a corresponding file")))
  (defun owen-find-corresponding-file-other-window ()
    "Find the file that corresponds to this one."
    (interactive)
    (find-file-other-window buffer-file-name)
    (owen-find-corresponding-file)
    (other-window -1))

  ; devenv.com error parsing
  (add-to-list 'compilation-error-regexp-alist 'owen-devenv)
  (add-to-list 'compilation-error-regexp-alist-alist '(owen-devenv
						       "*\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:see declaration\\|\\(?:warnin\\(g\\)\\|[a-z ]+\\) C[0-9]+:\\)"
						       2 3 nil (4)))

					; Turn on line numbers
					;(linum-mode)
  )



(defun owen-replace-string (FromString ToString)
  "Replace a string without moving point."
  (interactive "sReplace: \nsReplace: %s  With: ")
  (save-excursion
    (replace-string FromString ToString)
    ))
(define-key global-map [f8] 'owen-replace-string)

(add-hook 'c-mode-common-hook 'owen-big-fun-c-hook)
(add-hook 'c++-mode-common-hook 'owen-big-fun-c-hook)

(defun owen-save-buffer ()
  "Save the buffer after untabifying it."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (untabify (point-min) (point-max))))
  (save-buffer))

					; TXT mode handling
(defun owen-big-fun-text-hook ()
					; 4-space tabs
  (setq tab-width 4
        indent-tabs-mode nil)
  )
(add-hook 'text-mode-hook 'owen-big-fun-text-hook)


					; Commands
(set-variable 'grep-command "grep -irHn ")
(when *is-windows*
  (setq grep-use-null-device t)
  (set-variable 'grep-command "findstr -s -n -i -l "))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(auto-save-interval 0)
 '(auto-save-list-file-prefix nil)
 '(auto-save-timeout 0)
 '(auto-show-mode t t)
 '(delete-auto-save-files nil)
 '(delete-old-versions (quote other))
 '(imenu-auto-rescan t)
 '(imenu-auto-rescan-maxout 500000)
 '(kept-new-versions 5)
 '(kept-old-versions 5)
 '(make-backup-file-name-function (quote ignore))
 '(make-backup-files nil)
 '(mouse-wheel-follow-mouse nil)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (15)))
 '(version-control nil))


(defun owen-never-split-a-window
    "Never, ever split a window.  Why would anyone EVER want you to do that??"
  nil)
(setq split-window-preferred-function 'owen-never-split-a-window)


(global-visual-line-mode t)

(defun eval-region-or-buffer ()
  (interactive)
  (let ((debug-on-error t))
    (cond
     (mark-active
      (call-interactively 'eval-region)
      (message "Region evaluated!")
      (setq deactivate-mark t))
     (t
      (eval-buffer)
      (message "Buffer evaluated!")))))

;(add-hook 'emacs-lisp-mode-hook
;          (lambda ()
;            (local-set-key (kbd "C-m") 'eval-region-or-buffer)))


;; sample use of emacs abbreviation feature
(define-abbrev-table 'global-abbrev-table '(
	;; TODO, Notes
	("8todo" "TODO(Owen): ")
	("8note" "NOTE(Owen): ")
	("8imp" "IMPORTANT(Owen): ")
	("8fix" "FIXME(Owen): ")

    ;; emacs regex
    ("8d" "\\([0-9]+?\\)")
    ("8str" "\\([^\"]+?\\)\"")

    ;; shell commands
    ("8ditto" "ditto -ck --sequesterRsrc --keepParent src dest")
    ("8im" "convert -quality 85% ")

    ("8f0" "find . -type f -size 0 -exec rm {} ';'")
    ("8rsync" "rsync -z -r -v -t --exclude=\"*~\" --exclude=\".DS_Store\" --exclude=\".bash_history\" --exclude=\"**/xx_xahlee_info/*\"  --exclude=\"*/_curves_robert_yates/*.png\" --exclude=\"logs/*\"  --exclude=\"xlogs/*\" --delete --rsh=\"ssh -l xah\" ~/web/ xah@example.com:~/")
    ))

;; stop asking whether to save newly added abbrev when quitting emacs
(setq save-abbrevs nil)

;; turn on abbrev mode globally
(setq-default abbrev-mode t)


(provide 'init-programming)
