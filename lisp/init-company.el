(require-maybe 'company)

(add-hook 'after-init-hook 'global-company-mode)

(with-eval-after-load 'company
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 3)
  (setq company-show-numbers t)
  (setq company-echo-delay 0)
  (setq company-require-match nil)
  ;; make previous/next selection in the popup cycles
  (setq company-selection-wrap-around t)
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)
  ;; bigger popup window
  (setq company-tooltip-limit 20)
  ;;(set-face-attribute 'company-tooltip nil :foreground "magenta")
  
  (setq company-dabbrev-code-everywhere t) ;;Non-nil to offer completions in comments and strings.
  (setq company-dabbrev-minimum-length 3) ;;The minimum length for the completion candidate to be included.
  (setq company-dabbrev-other-buffers t) ;;If t, 'company-dabbrev' search buffers with the same major mode
  (setq company-dabbrev-downcase nil) ;;是否把候选项前面的字母转为小写
  (setq company-dabbrev-ignore-case t) ;;使选择后，前面输入的字母也转为正确的大小写
  (setq company-dabbrev-char-regexp "[\\.0-9a-z-_'/]") ;adjust regexp make `company-dabbrev' search words like `dabbrev-expand'

  (setq company-dabbrev-code-ignore-case t)
  (setq company-dabbrev-code-other-buffers 't) ;If t, search buffers with the same major mode.

  (setq company-etags-ignore-case t)
  
  ;; set default backends
  ;; company-dabbrev is for current buffer string auto complete
  (setq company-backends '(company-dabbrev company-dabbrev-code company-files))

  (defun eye-make-local-company-backends (backends modmap)
    "设置company-backends并设置相应major mode map下的按键，由于define-key第一个参数不能是symbol，需要在hook函数中使用"
    (setq company-backends backends)
    (when (keymapp modmap)
      (define-key modmap (kbd "<backtab>") 'company-manual-begin)
      (define-key modmap (kbd "<S-tab>") 'company-manual-begin)))
  
  (defun eye-setup-company-backends ()
    "Setup company-backends for major-mode"
    (cond ((eq major-mode 'css-mode) (eye-make-local-company-backends '(company-css company-dabbrev company-dabbrev-code company-keywords)
								      css-mode-map))
	  ((eq major-mode 'c-mode) (eye-make-local-company-backends '(company-etags company-dabbrev company-dabbrev-code company-keywords)
								    c-mode-map))
	  ((eq major-mode 'c++-mode) (eye-make-local-company-backends '(company-etags company-dabbrev company-dabbrev-code company-keywords)
								      c++-mode-map))
	  ((eq major-mode 'emacs-lisp-mode) (eye-make-local-company-backends '(company-elisp company-files company-dabbrev company-dabbrev-code company-keywords)
									     emacs-lisp-mode-map))
	  ((eq major-mode 'lisp-interaction-mode) (eye-make-local-company-backends '(company-elisp company-files company-dabbrev company-dabbrev-code company-keywords)
										   lisp-interaction-mode-map))
	  ((equal major-mode 'nxml-mode) (eye-make-local-company-backends '(company-nxml company-dabbrev company-dabbrev-code)
									  nxml-mode-map))
	  (t (eye-make-local-company-backends '(company-dabbrev company-dabbrev-code company-files)
					      global-map))
	  ))

  (dolist (hook '(c-mode-hook c++-mode-hook emacs-lisp-mode-hook lisp-interaction-mode-hook css-mode-hook nxml-mode-hook))
    (add-hook hook 'eye-setup-company-backends))

  (define-key company-active-map (kbd "M-i") 'company-select-previous)
  (define-key company-active-map (kbd "M-k") 'company-select-next)
  (define-key company-active-map (kbd "<tab>") 'company-select-next)
  (define-key company-active-map (kbd "TAB") 'company-select-next)
  (define-key company-active-map (kbd "<S-tab>") 'company-select-previous)
  )

;; Support yas in commpany
;; Note: Must be the last to involve all backends
;; (defvar company-enable-yas t
;;   "Enable yasnippet for all backends.")

;; (defun company-backend-with-yas (backend)
;;   (if (or (not company-enable-yas)
;;           (and (listp backend) (member 'company-yasnippet backend)))
;;       backend
;;     (append (if (consp backend) backend (list backend))
;;             '(:with company-yasnippet))))

;; (setq company-backends (mapcar #'company-backend-with-yas company-backends))


;; (use-package company-statistics
;; :ensure t
;; :init
;; (let ((dir "~/cache"))
;; (if (not (file-exists-p dir))
;; (make-directory dir))
;; (setq company-statistics-file (concat dir "/company-statistics-cache.el")))
;; (company-statistics-mode))

;; (require 'qml-mode)
;; (require 'company-qml)
;;   (add-hook 'qml-mode
;;             '(lambda ()
;;                (require 'company-qml)
;;                (add-to-list 'company-backends 'company-qml)))


;; (if (>= emacs-major-version 26)
;;     (progn
;;       (require 'company-posframe)
;;       (company-posframe-mode 1)
;;       ;; Let desktop.el not record the company-posframe-mode
;;       (push '(company-posframe-mode . nil)
;;             desktop-minor-mode-table)))


(defhydra+ hydra-funcs (:idle 1.0)
  ("g" global-company-mode "Company" :exit t))



(provide 'init-company)
