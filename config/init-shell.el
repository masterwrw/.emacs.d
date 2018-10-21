(require 'aweshell)

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
