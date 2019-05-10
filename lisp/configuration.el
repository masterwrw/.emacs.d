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
            (if (not (equal t (server-running-p)))
	      (server-start))))

(defun eye-require (feature msg)
  (eye--reset-time)
  (require feature)
  (eye--print-time msg))

;;;; basic configuration
(eye-require 'init-packages "packages")
(eye-require 'init-keys "keys")
(eye-require 'init-misc "misc")
(eye-require 'init-font "font")
(eye-require 'init-history "history")
(eye-require 'init-backup "backup")
(eye-require 'init-encoding "encoding")
(eye-require 'init-modeline "modeline")
(eye-require 'init-bookmark "bookmark")
(eye-require 'init-ido "ido")
(eye-require 'init-smex "smex")
(eye-require 'init-imenu "imenu")
(eye-require 'init-dired "dired")
(eye-require 'init-locale "locale")
;;;; site packages
(eye-require 'which-key "which-key")
(with-eval-after-load 'which-key (which-key-mode))
(when is-gui (eye-require 'init-theme "theme"))
;;(when is-gui (eye-require 'init-doom "doom"))
(eye-require 'init-avy "avy")
(eye-require 'init-avy-zap "avy-zap")
(eye-require 'init-ace-jump "ace-jump")
(eye-require 'init-idomenu "idomenu")
(eye-require 'init-ivy "ivy")
(eye-require 'init-super-save "super-save")
(eye-require 'init-eno "eno")
(eye-require 'init-bm "bm")
(eye-require 'init-ibuffer "ibuffer")
(eye-require 'init-hungry-delete "hungry-delete")
(eye-require 'init-rainbow "rainbow")
(eye-require 'init-aweshell "aweshell")
(eye-require 'init-web-search "web-search")
(eye-require 'init-watch-other-window "watch-other-window")
(eye-require 'init-rg "rg")
(eye-require 'init-color-rg "color-rg")
(eye-require 'init-elisp "elisp")
(eye-require 'init-orgmode "orgmode")
(eye-require 'init-yankpad "yankpad")
(eye-require 'init-deft "deft")
(eye-require 'init-cpp "cpp")
(eye-require 'init-php "php")
(eye-require 'init-qt "qt")
(eye-require 'init-ctags "ctags")
(eye-require 'init-counsel-etags "counsel-etags")
(eye-require 'init-company "company")
(eye-require 'init-company-quickhelp "company-quickhelp")
(eye-require 'init-ffit "ffit")
(eye-require 'init-ffip "ffip")
(eye-require 'init-external "external")
(when is-linux (eye-require 'init-magit "magit"))

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
