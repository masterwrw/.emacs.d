(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))
(load-theme 'my-dark t)


;; 自定义一些关键词高亮
(setq fixme-modes '(c++-mode c-mode emacs-lisp-mode python-mode))
(make-face 'font-lock-fixme-face)
(make-face 'font-lock-study-face)
(make-face 'font-lock-important-face)
(make-face 'font-lock-improve-face)
(make-face 'font-lock-note-face)
(mapc (lambda (mode)
		(font-lock-add-keywords
         mode
         '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
           ("\\<\\(STUDY\\)" 1 'font-lock-study-face t)
           ("\\<\\(IMPROVE\\)" 1 'font-lock-improve-face t)
           ("\\<\\(IMPORTANT\\)" 1 'font-lock-important-face t)
           ("\\<\\(NOTE\\)" 1 'font-lock-note-face t))))
      fixme-modes)
(modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-study-face "Green" nil nil t nil t nil nil)
(modify-face 'font-lock-improve-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-important-face "Green" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "Dark Green" nil nil t nil t nil nil)


(defun eye/qtcreator-dark-theme ()
  (interactive)
  (set-face-background 'region "#1d545c") ;; 选中区域背景
  (set-face-foreground 'region "#bec0c2") ;; 选中区域前景
  (set-face-attribute 'mode-line nil :background "#2e2f30" :foreground "#a8abb0")
  (set-face-background 'hl-line "#454545")
  (set-face-attribute 'font-lock-function-name-face nil :foreground "#d6cf79" :bold t)
  (set-face-attribute 'company-template-field nil :background "#c7edcc" :foreground "black")
  (set-face-attribute 'company-scrollbar-bg nil :background "black")
  (set-face-attribute 'company-scrollbar-fg nil :background "#c7edcc")
  (set-face-attribute 'company-tooltip-selection nil :background "#c7edcc")
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default ((t (:background "#2e2f30" :foreground "#d6cf89")))) ;; 全局背景和前景
   '(font-lock-builtin-face ((((class color) (min-colors 88) (background dark)) (:foreground "RosyBrown4"))))
   '(font-lock-comment-face ((((class color) (min-colors 88) (background dark)) (:foreground "#a8abb0"))))
   '(font-lock-constant-face ((((class color) (min-colors 88) (background dark)) (:foreground "#ff8080"))))
   '(font-lock-doc-face ((t (:inherit font-lock-string-face :foreground "khaki4"))))
   '(font-lock-keyword-face ((((class color) (min-colors 88) (background dark)) (:foreground "#45c6d6" :weight bold))))
   '(font-lock-preprocessor-face ((t (:inherit font-lock-builtin-face :foreground "#ff6aad" :weight bold))))
   '(font-lock-string-face ((((class color) (min-colors 88) (background dark)) (:foreground "#d69545"))))
   '(font-lock-type-face ((((class color) (min-colors 88) (background dark)) (:foreground "#ff8080"))))
   '(font-lock-variable-name-face ((((class color) (min-colors 88) (background dark)) (:foreground "#d6bb9a"))))
   '(font-lock-warning-face ((((class color) (min-colors 88) (background dark)) (:foreground "red" :weight bold))))
   )
  )

(defun eye/idea-light-theme ()
  (interactive)
  (set-face-attribute 'default nil :background "#efefef" :foreground "black")
  (set-face-attribute 'region nil :background "#3399ff" :foreground "white") ;; 选中区域
  (set-face-attribute 'mode-line nil :background "#2e2f30" :foreground "#a8abb0")
  (set-face-attribute 'hl-line nil :background "#e3f6d2")
  (set-face-attribute 'font-lock-function-name-face nil :foreground "#2f2f2f" :bold t)
  (set-face-attribute 'font-lock-builtin-face nil :foreground "RosyBrown4")
  (set-face-attribute 'font-lock-comment-face nil :foreground "#8c8080")
  (set-face-attribute 'font-lock-constant-face nil :foreground "black")
  (set-face-attribute 'font-lock-keyword-face nil :foreground "#000080" :bold t)
  (set-face-attribute 'font-lock-doc-face nil :foreground "khaki4")
  (set-face-attribute 'font-lock-string-face nil :foreground "#008000" :bold t)
  (set-face-attribute 'font-lock-preprocessor-face nil :foreground "#000080")
  (set-face-attribute 'font-lock-type-face nil :foreground "black")
  (set-face-attribute 'font-lock-variable-name-face nil :foreground "#660e7a")
  (set-face-attribute 'font-lock-warning-face nil :foreground "red")
  )



(provide 'init-theme)
