(require 'ryo-modal)
(require 'which-key)

(setq which-key-enable-extended-define-key t)
(which-key-mode)

;; for replace which key tip: +prefix
(push '((nil . "ryo:.*:") . (nil . "")) which-key-replacement-alist)


(defun ryo-modal-mode-on ()
  (interactive)
  (ryo-modal-mode 1)
  (set-cursor-color "#44aa33"))

(defun ryo-modal-mode-off ()
  (interactive)
  (ryo-modal-mode -1)
  (set-cursor-color "#ee44a3"))

(defun setup-ryo-key ()
  (global-set-key (kbd "<home>") 'ryo-modal-mode)
  (define-key key-translation-map (kbd "ESC") (kbd "<home>"))

  (let ((mode-hooks '(find-file-hook
              message-mode-hook
              dired-mode-hook
              help-mode-hook
              man-mode-hook
              prog-mode-hook
              helpful-mode-hook
              ;; c++-mode-hook
              ;; emacs-lisp-mode-hook
              ;; org-mode-hook
              ;; css-mode-hook
              ;; python-mode-hook
              )))
    (dolist (var mode-hooks)
      (add-hook var 'ryo-modal-mode-on)))

  
  ;; (let ((excludes '(magit-status-mode-hook text-mode)))
    ;; (dolist (var excludes)
      ;; (add-hook var 'ryo-modal-mode-off))
    ;; )
  )

(run-with-idle-timer 10 0
             #'(lambda ()
             (when (member major-mode
                     '(dired-mode
                       help-mode
                       man-mode
                       woman-mode
                       c++-mode
                       python-mode
                       php-mode
                       emacs-lisp-mode
                       helpful-mode))
                 (ryo-modal-mode-on))))


(require 'ryo-modal)
(setup-ryo-key)
(set-cursor-color "#ee44a3")
(setq ryo-modal-cursor-type 'box)
(setq ryo-modal-cursor-color "#44aa33")
(setq ryo-modal-default-cursor-color "#ee44a3")

(ryo-modal-mode-on)

(ryo-modal-keys
 ("j" left-char)
 ("l" right-char)
 ("u" left-word)
 ("o" right-word)
 ("i" previous-line)
 ("k" next-line)
 ("h" eye/beginning-of-line-or-block)
 (";" xah-end-of-line-or-block)
 ("'" recenter-top-bottom)
 ("n" scroll-up-command)
 ("p" scroll-down-command)
 ("/" xah-comment-dwim)

 ("m" set-mark-command)
 ("w" xah-next-window-or-frame)
 ("r" query-replace)

 ("8" xah-extend-selection)
 ("s" eye/save-buffer)
 
 ("c" xah-copy-line-or-region)
 ("x" xah-cut-line-or-region)
 ("v" yank-with-indent)
 ("z" undo)

 ("q" mode-line-other-buffer)

 ;; delete
 ("dd" delete-line-no-copy :name "Delete Line")
 ("dl" delete-char :name "Delete Forward Char")
 ("du" delete-inner-word-no-copy :name "Delete Backword Word")
 ("do" delete-forward-word-no-copy :name "Delete Forward Word")
 ("d;" delete-end-of-line-no-copy :name "Delete Line End")
 ("dh" delete-beginning-of-line-no-copy :name "Delete Line Begin")
 ("dj" delete-backward-char :name "Delete Backword Char")
 ("dw" delete-window)
 )

(global-unset-key (kbd "<f1>"))
(global-unset-key (kbd "<f2>"))
(global-unset-key (kbd "<f3>"))
(global-unset-key (kbd "<f4>"))
(global-unset-key (kbd "<f5>"))
(global-unset-key (kbd "<f6>"))
(global-unset-key (kbd "<f7>"))
(global-unset-key (kbd "<f8>"))
(global-unset-key (kbd "<f9>"))
(global-unset-key (kbd "<f10>"))
(global-unset-key (kbd "<f11>"))
(global-unset-key (kbd "<f12>"))


(define-key global-map (kbd "<M-up>") 'scroll-up-defun-or-lines)
(define-key global-map (kbd "<M-down>") 'scroll-down-defun-or-lines)
(define-key global-map (kbd "<M-left>") 'backward-word)
(define-key global-map (kbd "<M-right>") 'forward-word)

(define-key global-map (kbd "<C-up>") 'scroll-down-command)
(define-key global-map (kbd "<C-down>") 'scroll-up-command)

;; 不设置为全局,否则影响minibuffer输入
;; (define-key prog-mode-map (kbd "<tab>") 'indent-or-expand)
(define-key prog-mode-map (kbd "<tab>") 'hippie-expand)
(define-key prog-mode-map (kbd "<C-tab>") 'mode-line-other-buffer)

(define-key global-map (kbd "<C-tab>") 'mode-line-other-buffer)
(define-key global-map (kbd "<backtab>") 'indent-for-tab-command)

(define-key global-map (kbd "<f9> b") 'bookmark-bmenu-list)

(defalias 'backward-kill-word 'eye/kill-inner-word)
(define-key global-map (kbd "<M-backspace>") 'eye/kill-inner-word)
(define-key global-map (kbd "<C-backspace>") 'eye/kill-inner-word)

(define-key global-map (kbd "M-c") 'eye/capitalize-word)
(define-key global-map (kbd "M-u") 'eye/upcase-word)

(define-key global-map (kbd "C-,") 'other-window)

(define-key global-map (kbd "<C-wheel-up>") 'text-scale-decrease)
(define-key global-map (kbd "<C-wheel-down>") 'text-scale-increase)

(provide 'init-ryo)
