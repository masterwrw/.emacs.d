;;; keyfreq
;; http://blog.binchen.org/posts/how-to-be-extremely-efficient-in-emacs.html
;; https://github.com/redguardtoo/emacs.d/blob/master/lisp/init-keyfreq.el

(require-package 'keyfreq)
(require 'keyfreq)

(defun turnon-keyfreq-mode ()
  (interactive)
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(defun turnoff-keyfreq-mode ()
  (interactive)
  (keyfreq-mode -1)
  (keyfreq-autosave-mode -1))

(setq keyfreq-excluded-commands
      '(self-insert-command
	abort-recursive-edit
	forward-char
	backward-char
	previous-line
	next-line
	mwhell-scroll
	windmove-left
	windmove-right
	windmove-down
	windmove-up
	move-end-of-line
	save-buffer))


(unless (file-exists-p (file-truename keyfreq-file))
  (with-temp-buffer
    (insert "()")
    (write-file (file-truename keyfreq-file))))


(turnon-keyfreq-mode)

(provide 'init-keyfreq)
