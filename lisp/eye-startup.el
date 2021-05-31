
;;;; startup
(setq gc-cons-threshold (* 100 1000 1000)) ;;100MB
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after init."
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold (* 10 1000 1000)) ;;10MB
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'focus-out-hook 'garbage-collect))))


;; Do not use garbage-collect when use minibuffer
;; @see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(defun eye-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun eye-minibuffer-exit-hook ()
  (setq gc-cons-threshold (* 10 1000 1000))) ;;10MB

(add-hook 'minibuffer-setup-hook #'eye-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'eye-minibuffer-exit-hook)

;;;; system version
(setq is-windows (or
		  (eq system-type 'windows-nt)
		  (eq system-type 'cygwin)))
(setq is-linux (eq system-type 'gnu/linux))
(setq is-mac (eq system-type 'darwin))

(setq is-gui (display-graphic-p))
(setq is-terminal (not (display-graphic-p)))

;;;; custom file
;; must load custom-file before all init-* config
(setq custom-file
      (concat user-emacs-directory
	      (if is-windows "custom-set-variables-win.el" "custom-set-variables-linux.el")))

(defun touch (path)
  (unless (file-exists-p custom-file)
    (with-temp-buffer
      (write-file path))))

(touch custom-file)
(load custom-file t t)

;;;; startup time
(defvar startup-time (current-time))
(defvar begin-time nil "This is for calc require package time")
(defun eye-print-startup-time ()
  (message (format
	    "\n\nEmacs startup time: %.6f seconds.\n\n\n"
	    (- (float-time (current-time)) (float-time startup-time)))))
(add-hook 'after-init-hook #'eye-print-startup-time)


(provide 'eye-startup)
