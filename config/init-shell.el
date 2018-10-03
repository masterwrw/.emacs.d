(when (eq system-type 'windows-nt)
  (global-set-key (kbd "<S-return>") 'shell))

(when (not (eq system-type 'windows-nt))
  (require 'multi-term)
  ;; no limit buffer length
  (setq term-buffer-maximum-size 0)
  ;; use bash or zsh
  (if (executable-find "zsh")
      (setq multi-term-program "/bin/zsh")
    (setq multi-term-program "/bin/bash")))


;;; eshell
(defun eye/eshell-clear ()
  "Clear eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(add-hook 'eshell-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "C-l") 'eye/eshell-clear)))


(defun eye/shell-cmd (buffer env)
  "Run cmd with new buffer name and path environment."
  (let ((explicit-shell-file-name "C:\\Windows\\System32\\cmd.exe")
        (shell-path-bak (getenv "PATH")) ;; save path
        (shell-buffer-name buffer)
        (shell-path-cmd env))
    (setenv "PATH" (concat shell-path-cmd "C:\\Windows\\System32;"))
    (shell shell-buffer-name)
    ;; restore path
    (setenv "PATH" shell-path-bak)))




(provide 'init-shell)
