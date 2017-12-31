(package-initialize)

(defvar best-gc-cons-threshold 4000000 "Best default gc threshold value. Should't be too big.")
;; don't GC during startup to save time
(setq gc-cons-threshold most-positive-fixnum)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

(setq *is-a-mac* (eq system-type 'darwin))
(setq *win64* (eq system-type 'windows-nt))
(setq *cygwin* (eq system-type 'cygwin))
(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)))
(setq *unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-linux)))
(setq *emacs24* (and (not (featurep 'xemacs)) (or (>= emacs-major-version 24))))
(setq *emacs25* (and (not (featurep 'xemacs)) (or (>= emacs-major-version 25))))
(setq *no-memory* (cond
                   (*is-a-mac*
                    (< (string-to-number (nth 1 (split-string (shell-command-to-string "sysctl hw.physmem")))) 4000000000))
                   (*linux* nil)
                   (t nil)))

(setq *emacs24old*  (or (and (= emacs-major-version 24) (= emacs-minor-version 3))
                        (not *emacs24*)))

;; @see https://www.reddit.com/r/emacs/comments/55ork0/is_emacs_251_noticeably_slower_than_245_on_windows/
;; Emacs 25 does gc too frequently
(when *emacs25*
  ;; (setq garbage-collection-messages t) ; for debug
  (setq gc-cons-threshold (* 64 1024 1024) )
  (setq gc-cons-percentage 0.5)
  (run-with-idle-timer 5 t #'garbage-collect))

;; @see https://www.reddit.com/r/emacs/comments/4q4ixw/how_to_forbid_emacs_to_touch_configuration_files/
(setq custom-file (concat user-emacs-directory "custom-set-variables.el"))
(load custom-file 'noerror)

;; init files
(require 'init-autoload)
(require 'init-site-lisp) ;; Must come before elpa
(require 'init-elpa)
(require 'init-gui-frames)
(require 'init-cnfonts)
(require 'init-color-theme)
(require 'init-helm-ag)
(require 'init-company)
(require 'init-yasnippet)
(require 'init-smex)
(require 'init-ido)
(require 'init-autopair)
(require 'init-keymap)
(require 'init-bind-key)
(require 'init-misc)

(server-start)
