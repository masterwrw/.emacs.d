(defun eye/push-to-list (member lst)
  "添加成员到列表，如果已经有了，则不重复添加"
  (unless (memq member lst)
    (push member lst)))



(use-package company
  :ensure t
  :bind (("C-c C-y" . company-yasnippet)
	 :map company-active-map
	 ("C-p" . company-select-previous)
	 ("C-n" . company-select-next)
	 :map company-search-map
	 ("C-p" . company-select-previous)
	 ("C-n" . company-select-next))
  ;;:init
  ;;(add-hook 'after-init-hook #'global-company-mode)
  :config
  (require 'company-yasnippet)
  (require 'company-dabbrev)
  (require 'company-css)
  (require 'company-files)
  (require 'desktop)
  (use-package company-posframe :ensure t
    :config
    (company-posframe-mode 1)
    ;; Let desktop.el not record the company-posframe-mode
    (push '(company-posframe-mode . nil)
	  desktop-minor-mode-table))
  
  (global-company-mode)
  
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2)
  (setq company-show-numbers t)
  (setq company-echo-delay 0)
  (setq company-require-match nil)

  (setq company-dabbrev-code-everywhere t)
  (setq company-dabbrev-minimum-length 2)
  (setq company-dabbrev-other-buffers 'all)
  (setq company-dabbrev-downcase nil)
  ;; make previous/next selection in the popup cycles
  ;; (setq company-selection-wrap-around t)

  (setq company-dabbrev-char-regexp "[\\.0-9a-z-_'/]") ;adjust regexp make `company-dabbrev' search words like `dabbrev-expand'
  (setq company-dabbrev-code-other-buffers 'all) ;search completion from all buffers, not just same mode buffers.
  
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)
  ;; bigger popup window
  (setq company-tooltip-limit 20)
  (set-face-attribute 'company-tooltip nil :foreground "magenta")

  ;; backends
  (setq company-backends nil)
  
  (eye/push-to-list 'company-css company-backends)
  (eye/push-to-list 'company-files company-backends)
  (eye/push-to-list 'company-etags company-backends)
  ;; company-dabbrev config, it is for current buffer string auto complete
  (eye/push-to-list 'company-dabbrev company-backends)
  (eye/push-to-list 'company-dabbrev-code company-backends)

  (add-hook 'emacs-lisp-mode-hook
		 '(lambda ()
		    (require 'company-elisp)
		    (eye/push-to-list 'company-lisp company-backends)))

  
  ;; Support yas in commpany
  ;; Note: Must be the last to involve all backends
  (defvar company-enable-yas t
    "Enable yasnippet for all backends.")

  (defun company-backend-with-yas (backend)
    (if (or (not company-enable-yas)
            (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (setq company-backends (mapcar #'company-backend-with-yas company-backends))

  
  
  (use-package company-statistics
    :ensure t
    :init
    (let ((dir "~/cache"))
      (if (not (file-exists-p dir))
          (make-directory dir))
      (setq company-statistics-file (concat dir "/company-statistics-cache.el")))
    (company-statistics-mode)))




(provide 'init-company-mode)
