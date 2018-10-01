(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'yas-minor-mode)
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (require 'company-elisp)
	    (add-to-list 'company-backends 'company-elisp)))


(define-key emacs-lisp-mode-map (kbd "<M-up>") 'beginning-of-defun)
(define-key emacs-lisp-mode-map (kbd "<M-down>") 'end-of-defun)



(provide 'init-elisp)
