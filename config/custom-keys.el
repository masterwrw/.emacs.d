(ryo-modal-keys
 ("SPC "(
	 ("d" dired-jump)
	 ("gc" avy-goto-char :name "Goto Char")
	 ("gl" avy-goto-line :name "Goto Line")
	 ("jd" dumb-jump-go)
	 ("wl" windmove-right)
	 ("wj" windmove-left)
	 ("wi" windmove-up)
	 ("wk" windmove-down)
	 ("wu" winner-undo)
	 ("i" imenu)
	 ("bl" bookmark-bmenu-list)
	 ("fa" beginning-of-defun :name "Begin Fun")
	 ("fe" end-of-defun :name "End Fun")
	 ("bb" beginning-of-buffer)
	 ("be" end-of-buffer)
	 ("bk" kill-current-buffer)
	 ("fo" find-file-other-window)
	 ("ff" counsel-find-file)
	 )))

(ryo-modal-key "," 'backward-forward-previous-location)
(ryo-modal-key "." 'backward-forward-next-location)

;; ("1" delete-other-windows)
;; ("2" split-window-below)
;; ("3" split-window-right)



(provide 'custom-keys)
