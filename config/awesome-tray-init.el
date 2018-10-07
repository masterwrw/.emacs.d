(require 'awesome-tray)

;; add to show file coding
(defun awesome-tray-build-info ()
  (let ((info ""))
    ;; Collection information.
    (mapcar '(lambda (i) (setq info (format " %s %s" info i)))
            (list
             ;; Git branch.
             (if (fboundp 'magit-get-current-branch)
                 (let ((branch (magit-get-current-branch)))
                   (if branch
                       (format "Git:%s" branch)
                     ""))
               "")
             ;; Current mode.
             major-mode
             ;; Location.
             (format "(%s:%s)" (line-number-at-pos) (current-column))
	     ;; Coding
	     (format "%s" buffer-file-coding-system) ;; added by custom
             ;; Date.
             (format-time-string "[%Y-%m-%d %H:%M]")))
    ;; Add color property.
    (put-text-property 0 (length info) 'face 'awesome-tray-info-face info)
    info))

(awesome-tray-mode 1)



(provide 'awesome-tray-init)
