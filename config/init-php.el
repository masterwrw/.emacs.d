(require 'php-mode)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
;; fix irony warning when open php file
;; (push 'php-mode irony-supported-major-modes)
(defun eye/switch-php-html-mode ()
  "Switch the php-mode and html-mode"
  (interactive)
  (cond ((string= mode-name "html")
	 (php-mode))
	((string= mode-name "php")
	 (html-mode))))

(require 'company-php)
(add-hook 'php-mode
	  '(lambda ()
	     (add-to-list 'company-backends 'company-php)))


(provide 'init-php)
