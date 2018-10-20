(ryo-modal-keys
 ("SPC "(
	 ("SPC" ryo-modal-mode-off)
	 ("c" eye/eno-copy)
	 ("v" counsel-yank-pop)
	 ("d" dired-jump)
	 ("k" kill-current-buffer)
	 ("gc" avy-goto-char :name "Goto Char")
	 ("gl" avy-goto-line :name "Goto Line")
	 ("jd" dumb-jump-go)
	 ("wu" winner-undo)
	 ("i" counsel-imenu)
	 ("bl" bookmark-bmenu-list)
	 ("fa" beginning-of-defun :name "Begin Fun")
	 ("fe" end-of-defun :name "End Fun")
	 ("bb" beginning-of-buffer)
	 ("be" end-of-buffer)
	 )))

(ryo-modal-key "a" 'counsel-M-x)
(ryo-modal-key "e" 'counsel-ibuffer)
(ryo-modal-key "ff" 'counsel-find-file)
(ryo-modal-key "fo" 'find-file-other-window)
(ryo-modal-key "," 'backward-forward-previous-location)
(ryo-modal-key "." 'backward-forward-next-location)


;; 这里的 list 不能使用 quote 或 ' 因为 define-key 的第一个参数不是一个 symbol
(dolist (modmap (list global-map c++-mode-map))
	(progn
	  (define-key modmap (kbd "M-j") 'left-char)
	  (define-key modmap (kbd "M-l") 'right-char)
	  (define-key modmap (kbd "M-u") 'left-word)
	  (define-key modmap (kbd "M-o") 'right-word)
	  (define-key modmap (kbd "M-i") 'previous-line)
	  (define-key modmap (kbd "M-k") 'next-line)
	  (define-key modmap (kbd "M-h") 'eye/beginning-of-line-or-block)
	  (define-key modmap (kbd "M-;") 'xah-end-of-line-or-block)
	  (define-key modmap (kbd "M-'") 'recenter-top-bottom)
	  (define-key modmap (kbd "M-n") 'scroll-up-command)
	  (define-key modmap (kbd "M-p") 'scroll-down-command)
	  (define-key modmap (kbd "M-/") 'xah-comment-dwim)
	  (define-key modmap (kbd "M-w") 'xah-next-window-or-frame)
	  ))


(provide 'custom-keys)
