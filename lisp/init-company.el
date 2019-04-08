;; (require 'company)
;; (require 'company-dabbrev)
;; (require 'company-files)
;; (require 'company-etags)
;;(require 'company-yasnippet)
;;(require 'company-css)

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-i") 'company-select-previous)
  (define-key company-active-map (kbd "M-k") 'company-select-next)
  (define-key company-active-map (kbd "<tab>") 'company-select-next)

  ;;(global-company-mode)

  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 3)
  (setq company-show-numbers t)
  (setq company-echo-delay 0)
  (setq company-require-match nil)

  (setq company-dabbrev-code-everywhere t)
  (setq company-dabbrev-minimum-length 3)
  (setq company-dabbrev-other-buffers 'all)
  (setq company-dabbrev-downcase nil)
  ;; make previous/next selection in the popup cycles
  (setq company-selection-wrap-around t)

  (setq company-dabbrev-char-regexp "[\\.0-9a-z-_'/]") ;adjust regexp make `company-dabbrev' search words like `dabbrev-expand'
  (setq company-dabbrev-code-other-buffers 'all) ;search completion from all buffers, not just same mode buffers.

  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)
  ;; bigger popup window
  (setq company-tooltip-limit 20)
  ;;(set-face-attribute 'company-tooltip nil :foreground "magenta")

  ;; backends
  (setq company-backends nil)

  ;; company-dabbrev config, it is for current buffer string auto complete
  (add-to-list 'company-backends 'company-dabbrev)
  (add-to-list 'company-backends 'company-dabbrev-code)
  (add-to-list 'company-backends 'company-files)
  ;;(add-to-list 'company-backends 'company-css)

  (with-eval-after-load 'cc-mode
    (add-to-list 'company-backends 'company-etags))
  (with-eval-after-load 'counsel-etags
    (add-to-list 'company-backends 'company-etags))

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
