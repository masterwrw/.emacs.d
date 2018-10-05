(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'yas-minor-mode)
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (require 'company-elisp)
	    (add-to-list 'company-backends 'company-elisp)))


(define-key emacs-lisp-mode-map (kbd "<M-up>") 'scroll-up-defun-or-lines)
(define-key emacs-lisp-mode-map (kbd "<M-down>") 'scroll-down-defun-or-lines)

(define-key emacs-lisp-mode-map (kbd "<f5>") 'eval-last-sexp)


(provide 'init-elisp)
