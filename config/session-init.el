(require 'base-toolkit)
(setq desktop-load-locked-desktop t) ;don't popup dialog ask user, load anyway

(defun emacs-session-restore ()
  "Restore emacs session."
  (interactive)
  (ignore-errors
    ;; Kill unused buffers.
    (kill-unused-buffers)
    ;; Restore session.
    (desktop-read user-cache-directory)
    ))

(defun emacs-session-save (arg)
  "Save emacs session."
  (interactive "p")
  (ignore-errors
    (if (equal arg 4)
        ;; Kill all buffers if with prefix argument.
        (mapc 'kill-buffer (buffer-list))
      ;; Kill unused buffers.
      (kill-unused-buffers)
      ;; Save all buffers before exit.
      (auto-save-buffers))
    ;; Save session.
    (make-directory user-cache-directory t)
    (desktop-save user-cache-directory)
    ;; Exit emacs.
    (kill-emacs)))


(global-set-key (kbd "<f3> d") 'emacs-session-save)
(global-set-key (kbd "C-x C-c") 'emacs-session-save)


(provide 'session-init)
