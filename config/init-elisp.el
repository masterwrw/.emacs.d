(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'yas-minor-mode)
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (require 'company-elisp)
	    (add-to-list 'company-backends 'company-elisp)))



(provide 'init-elisp)
