;;; configuration.el --- My emacs configuration -*- lexical-binding: t -*-

;; Produce backtraces when errors occur
(setq debug-on-error t)

(let ((minver "24.4"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "25.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))


(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;;; startup
;; Speed up startup
(setq gc-cons-threshold 80000000) ;;80MB
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after init."
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold 800000) ;;800KB
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
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'eye-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'eye-minibuffer-exit-hook)

;;
;; 启动时间统计
;;
;; 自定义计算时间
(defvar init-start (current-time))
(add-hook 'after-init-hook
          (lambda ()
            (message (format "\n\nEmacs startup time: %.6f seconds.\n\n\n" (- (float-time (current-time)) (float-time init-start))))
            ))

(defvar begin-time (current-time))
(defun eye--reset-time ()
  (setq begin-time (current-time)))

(defun eye--print-time (msg)
  "Print cost time from begin-time.
 use eye--reset-time to reset begin-time."
  (message
   (format "-----------------------------------------------%.6f sec: %s"
	   (- (float-time (current-time))
	      (float-time begin-time))
	   msg)))

;;;; system env
(setq is-windows (or
		  (eq system-type 'windows-nt)
		  (eq system-type 'cygwin)))
(setq is-linux (eq system-type 'gnu/linux))
(setq is-mac (eq system-type 'darwin))

(setq is-gui (display-graphic-p))
(setq is-terminal (not (display-graphic-p)))

;; 使用 emacsclient 需要先启动服务
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
	      (server-start))))

;;;; basic configuration
(require 'init-packages)
(require 'init-leader-key)
(require 'init-misc)
(require 'init-font)
(require 'init-history)
(require 'init-backup)
(require 'init-encoding)
(require 'init-modeline)
(require 'init-bookmark)
(require 'init-ido)
(require 'init-smex)
(require 'init-imenu)
(require 'init-dired)
(require 'init-locale)
;;;; site packages
(require 'which-key)
(with-eval-after-load 'which-key (which-key-mode))
(when is-gui (require 'init-theme))
(when is-gui (require 'init-doom))
(require 'init-avy)
(require 'init-avy-zap)
(require 'init-ace-jump)
(require 'init-idomenu)
(require 'init-ivy)
(require 'init-web-search)
(require 'init-watch-other-window)
(require 'init-rg)
(require 'init-elisp)
(require 'init-orgmode)
(require 'init-cpp)
(require 'init-counsel-etags)
(require 'init-company)
(require 'init-external)
(when is-linux (require 'init-magit))

;;;; load custom-file
(setq custom-file (concat user-emacs-directory "custom-set-variables.el"))
(unless (file-exists-p custom-file)
  (f-touch custom-file))
(load custom-file t t)



(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
