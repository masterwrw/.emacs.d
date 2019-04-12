(require 'solarized)
;; make the modeline high contrast
(setq solarized-high-contrast-mode-line nil)
;; Use less bolding
(setq solarized-use-less-bold t)
;; Use more italics
(setq solarized-use-more-italic t)

(load-theme 'solarized-dark t)



;; set some faces color
;; (set-face-attribute 'default nil :foreground "#daa520")
;; (set-face-attribute 'region nil :background "#0000ee")
;; (set-face-attribute 'fringe nil :background "#999999")
;; (set-face-attribute 'font-lock-comment-face nil :foreground "#ff6347") ;;注释内容
;; (set-face-attribute 'font-lock-comment-delimiter-face nil :foreground "#ff6347") ;;注释本身的符号

;; (add-hook 'after-init-hook
;; 	  (lambda ()
;; 	    (set-face-attribute 'mode-line nil :height 110)
;; 	    (set-face-attribute 'mode-line-inactive nil :height 110)))

;;(require 'spolsky-theme)
;; (load-theme 'spolsky t)

;; (require 'moe-theme)
;; (load-theme 'moe-dark t)


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



(provide 'init-theme)
