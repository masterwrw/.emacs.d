;;;; modeline
;; Copy from https://github.com/redguardtoo/emacs.d/blob/master/lisp/init-modeline.el
;; @see http://emacs-fu.blogspot.com/2011/08/customizing-mode-line.html
;; But I need global-mode-string,
;; @see http://www.delorie.com/gnu/docs/elisp-manual-21/elisp_360.html
;; use setq-default to set it for /all/ modes
(setq-default mode-line-format
              (list
               ;; the buffer name; the file name as a tool tip
               '(:eval (propertize (if (buffer-modified-p)
                                                                   "%b* "
                                                                 "%b ")
                                                           'face nil
                                   'help-echo (buffer-file-name)))

               ;; line and column
               "(" ;; '%02' to set to 2 chars at least; prevents flickering
               "%02l" "," "%01c"
               ;; (propertize "%02l" 'face 'font-lock-type-face) ","
               ;; (propertize "%02c" 'face 'font-lock-type-face)
               ") "

               '(:eval (format "%s" buffer-file-coding-system))
               
               " "
               
               ;; the current major mode for the buffer.
               "["

               '(:eval (propertize "%m" 'face nil
                                   'help-echo buffer-file-coding-system))
               " "


               ;; insert vs overwrite mode, input-method in a tooltip
               '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                                   'face nil
                                   'help-echo (concat "Buffer is in "
                                                      (if overwrite-mode "overwrite" "insert") " mode")))

               ;; is this buffer read-only?
               '(:eval (when buffer-read-only
                         (concat ","  (propertize "RO"
                                                  'face nil
                                                  'help-echo "Buffer is read-only"))))
               "] "
	       "%n " ;; narrow state
	       
               ;;global-mode-string, org-timer-set-timer in org-mode need this
               (propertize "%M" 'face nil)

               " --"
               ;; i don't want to see minor-modes; but if you want, uncomment this:
               ;; minor-mode-alist  ;; list of minor modes
               "%-" ;; fill with '-'
               ))



;; Show modeline information on top header
;; (setq-default header-line-format mode-line-format) ; Copy mode-line
;; (setq-default mode-line-format nil) ; Remove mode-line
;;(set-face-attribute 'header-line nil :background "white" :foreground "black")

(which-function-mode)
(defun set-header-line ()
  (setq header-line-format
        '((which-function-mode ("" which-func-format " ")))))



(provide 'init-modeline)
