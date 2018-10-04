(setq electric-pair-pairs '(
							(?\{ . ?\})
							(?\( . ?\))
							(?\[ . ?\])
							(?\" . ?\")
							))
(electric-pair-mode t)
(show-paren-mode 1)

;; Show color of #hex format string.
(require 'rainbow-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-mode)

;; fix warning: ad-handle-definition: ‘er/expand-region’ got redefined
;; (setq ad-redefinition-action 'accept)
;; (use-package expand-region
  ;; :ensure t
  ;; :bind ("C-q" . er/expand-region)
  ;; )

(require 'hungry-delete)
(global-hungry-delete-mode)

;; save clipboard contents into kill-ring before replace theme
(setq save-interprogram-paste-before-kill t)

(require 'wdired)

;; 打开 .dired 后缀文件时，自动进入 dired-virtual-mode 模式。
(require 'dired-x)
(setq auto-mode-alist (cons '("[^/]\\.dired$" . dired-virtual-mode)
                            auto-mode-alist))

;; 如果开启了全局 global-auto-revert，则 dired-virtual-mode 模式下经常会弹出提示，所以只在编程模式下开启。
(add-hook 'prog-mode-hook
		  '(lambda ()
			 (auto-revert-mode 1)))



(require 'paredit)
;;(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
;;(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
;;(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
;;(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
;;(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
;;(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;;(add-hook 'scheme-mode-hook           #'enable-paredit-mode)


(require 'wgrep)
(require 'wgrep-ag)

;;; Kill buffers without asking
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))


;;; Do not ask
(setq ibuffer-expert t)

;;; 按行滚动
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

(require 'which-key)
(which-key-mode)


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

(require 'avy)
(ryo-modal-keys
 ("C-. "
  (("jj" avy-goto-char)
  ("jl" avy-goto-line))))
  

;; 自动保存书签
(add-hook 'kill-emacs-hook
          '(lambda ()
             (bookmark-save)))


(require 'multiple-cursors)


(delete-selection-mode 1)

;; 快速复制/剪切/移动其它位置的单词/行
(require 'eno)

;; writeroom
(require 'writeroom-mode)
(setq writeroom-width 120)

(defun writeroom-mode-on ()
  (interactive)
  (add-hook 'c++-mode-hook 'writeroom-mode)
  (add-hook 'emacs-lisp-mode-hook 'writeroom-mode)
  (add-hook 'org-mode-hook 'writeroom-mode)
  (add-hook 'css-mode-hook 'writeroom-mode)
  (writeroom-mode))

(defun writeroom-mode-off ()
  (interactive)
  (remove-hook 'c++-mode-hook 'writeroom-mode)
  (remove-hook 'emacs-lisp-mode-hook 'writeroom-mode)
  (remove-hook 'org-mode-hook 'writeroom-mode)
  (remove-hook 'css-mode-hook 'writeroom-mode)
  (writeroom-mode -1))

;;(use-package change-inner
;;  :ensure t)

(require 'vimish-fold)


;; (use-package centered-cursor-mode
  ;; :ensure t
  ;; :config
  ;; (add-hook 'c++-mode-hook 'centered-cursor-mode)
  ;; (add-hook 'emacs-lisp-mode-hook 'centered-cursor-mode)
  ;; (add-hook 'org-mode-hook 'centered-cursor-mode)
  ;; (add-hook 'css-mode-hook 'centered-cursor-mode))

(provide 'init-edit)
