(require-package 'smex)

;; smex or counsel-M-x?
(defvar my-use-smex nil
  "Use `smex' instead of `counsel-M-x' when press M-x.")

(defun my-M-x ()
  (interactive)
  (cond
    (my-use-smex
      (smex))
    ((fboundp 'counsel-M-x)
     ;; `counsel-M-x' will use `smex' to remember history
     (counsel-M-x))
    ((fboundp 'smex)
     (smex))
    (t
      (execute-extended-command))))
(global-set-key (kbd "M-x") 'my-M-x)
(global-set-key (kbd "C-x C-m") 'my-M-x)

(provide 'init-smex)
