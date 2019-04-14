(require 'helm)
(require 'helm-mode)
(helm-mode 1)

(defun eye/helm-hide-minibuffer ()
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))
(add-hook 'helm-minibuffer-set-up-hook 'eye/helm-hide-minibuffer)
(setq helm-autoresize-max-height 0
      helm-autoresize-min-height 40
      helm-M-x-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t
      helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match t
      helm-split-window-in-side-p nil
      helm-move-to-line-cycle-in-source nil
      helm-ff-search-library-in-sexp t
      helm-scroll-amount 8 
      helm-echo-input-in-header-line t)



;;(require 'helm-config)    
;;(helm-autoresize-mode 1)
;;(define-key helm-find-files-map (kbd "C-b") 'helm-find-files-up-one-level)
;;(define-key helm-find-files-map (kbd "C-f") 'helm-execute-persistent-action)

;;;; docs
(require 'helm-dash)
(setq helm-dash-browser-func 'eww)
(setq helm-dash-docsets-path "~/.docset")
(setq helm-dash-common-docsets '("C" "C++" "Qt_5" "Emacs_Lisp"))


(provide 'init-helm)
