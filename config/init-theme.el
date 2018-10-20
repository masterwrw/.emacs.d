;;; Custom theme configuration

(require 'moe-theme)
(add-to-list 'custom-theme-load-path "~/.emacs.d/site-lisp/moe-theme/")
(load-theme 'moe-dark t)
(show-paren-mode t)			;高亮括号
(setq show-paren-style 'expression)	;高亮括号整体内容

;; English font
(set-face-attribute
 'default nil
 :font (font-spec :family "Liberation Mono"
		  :weight 'normal
		  :slant 'normal
		  :size 14))
;; Chinese font
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font
   (frame-parameter nil 'font)
   charset
   (font-spec :family "Microsoft YaHei"
	      :weight 'normal
	      :slant 'normal
	      :size 9.5)))


;; Custom keyword hightlight
(setq fixme-modes '(c++-mode c-mode emacs-lisp-mode python-mode))
(make-face 'font-lock-fixme-face)
(make-face 'font-lock-study-face)
(make-face 'font-lock-important-face)
(make-face 'font-lock-improve-face)
(make-face 'font-lock-note-face)
(mapc (lambda (mode)
	(font-lock-add-keywords
         mode
         '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
           ("\\<\\(IMPROVE\\)" 1 'font-lock-improve-face t)
           ("\\<\\(STUDY\\)" 1 'font-lock-study-face t)
           ("\\<\\(NOTE\\)" 1 'font-lock-note-face t))))
      fixme-modes)
(modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-improve-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-study-face "#33aa00" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "#33aa00" nil nil t nil t nil nil)

;; (setq truncate-lines t) ;; 不自动折行


(provide 'init-theme)
