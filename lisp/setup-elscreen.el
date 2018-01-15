;;; Copy from https://stackoverflow.com/questions/22445670/save-and-restore-elscreen-tabs-and-split-frames
;; http://stackoverflow.com/questions/803812/emacs-reopen-buffers-from-last-session-on-startup
(defvar emacs-configuration-directory
    "~/.emacs.d/"
    "The directory where the emacs configuration files are stored.")
(defvar elscreen-tab-configuration-store-filename
    (concat emacs-configuration-directory ".elscreen")
    "The file where the elscreen tab configuration is stored.")


;; Auto save elscreen on exit
(defun elscreen-store ()
    "Store the elscreen tab configuration."
    (interactive)
    (if (desktop-save emacs-configuration-directory)
        (with-temp-file elscreen-tab-configuration-store-filename
            (insert (prin1-to-string (elscreen-get-screen-to-name-alist))))))

(push #'elscreen-store kill-emacs-hook)


;; Auto restore elscreen on startup
(defun elscreen-restore ()
    "Restore the elscreen tab configuration."
    (interactive)
    (if (desktop-read)
        (let ((screens (reverse
                        (read
                         (with-temp-buffer
                          (insert-file-contents elscreen-tab-configuration-store-filename)
                          (buffer-string))))))
            (while screens
                (setq screen (car (car screens)))
                (setq buffers (split-string (cdr (car screens)) ":"))
                (if (eq screen 0)
                    (switch-to-buffer (car buffers))
                    (elscreen-find-and-goto-by-buffer (car buffers) t t))
                (while (cdr buffers)
                    (switch-to-buffer-other-window (car (cdr buffers)))
                    (setq buffers (cdr buffers)))
                (setq screens (cdr screens))))))

;; (elscreen-restore)



(provide 'setup-elscreen)
